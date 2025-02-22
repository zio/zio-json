/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json

import zio.json.ast.Json
import zio.json.internal.{ FastStringWrite, SafeNumbers, Write }
import zio.json.javatime.serializers
import zio.{ Chunk, NonEmptyChunk }
import java.util.UUID
import scala.annotation._
import scala.collection.{ immutable, mutable }

trait JsonEncoder[A] extends JsonEncoderPlatformSpecific[A] {
  self =>

  /**
   * Returns a new encoder, with a new input type, which can be transformed to the old input type by the specified
   * user-defined function.
   */
  final def contramap[B](f: B => A): JsonEncoder[B] = new JsonEncoder[B] {
    override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
      self.unsafeEncode(f(b), indent, out)

    override def isNothing(b: B): Boolean = self.isNothing(f(b))

    override def isEmpty(b: B): Boolean = self.isEmpty(f(b))

    override final def toJsonAST(b: B): Either[String, Json] = self.toJsonAST(f(b))
  }

  /**
   * Returns a new encoder that can accepts an `Either[A, B]` to either, and uses either this encoder or the specified
   * encoder to encode the two different types of values.
   */
  final def either[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]] = JsonEncoder.either[A, B](self, that)

  /**
   * Returns a new encoder that can accepts an `Either[A, B]` to either, and uses either this encoder or the specified
   * encoder to encode the two different types of values. The difference with the classic `either` encoder is that the
   * resulting JSON has no field `Left` or `Right`. What should be: `{"Right": "John Doe"}` is encoded as `"John Doe"`
   */
  final def orElseEither[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    JsonEncoder.orElseEither[A, B](self, that)

  /**
   * Returns a new encoder with a new input type, which can be transformed to either the input type of this encoder, or
   * the input type of the specified encoder, using the user-defined transformation function.
   */
  final def eitherWith[B, C](that: => JsonEncoder[B])(f: C => Either[A, B]): JsonEncoder[C] =
    self.either(that).contramap(f)

  /**
   * Encodes the specified value into a JSON string, with the specified indentation level.
   */
  final def encodeJson(a: A, indent: Option[Int] = None): CharSequence = {
    val writePool = JsonEncoder.writePools.get
    try {
      val write = writePool.acquire()
      unsafeEncode(a, indent, write)
      write.toString
    } finally writePool.release()
  }

  /**
   * This default may be overridden when this value may be missing within a JSON object and still be encoded.
   */
  def isNothing(a: A): Boolean = false

  /**
   * This default may be overridden when this value may be empty within a JSON object and still be encoded.
   */
  def isEmpty(a: A): Boolean = false

  /**
   * Returns this encoder but narrowed to the its given sub-type
   */
  final def narrow[B <: A]: JsonEncoder[B] = self.asInstanceOf[JsonEncoder[B]]

  def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit

  /**
   * Converts a value to a Json AST
   *
   * The default implementation encodes the value to a Json byte stream and uses decode to parse that back to an AST.
   * Override to provide a more performant implementation.
   */
  def toJsonAST(a: A): Either[String, Json] = Json.decoder.decodeJson(encodeJson(a, None))

  /**
   * Returns a new encoder that is capable of encoding a tuple containing the values of this encoder and the specified
   * encoder.
   */
  final def zip[B](that: => JsonEncoder[B]): JsonEncoder[(A, B)] = JsonEncoder.tuple2(self, that)

  /**
   * Returns a new encoder that is capable of encoding a user-defined value, which is create from a tuple of the values
   * of this encoder and the specified encoder, from the specified user- defined function.
   */
  final def zipWith[B, C](that: => JsonEncoder[B])(f: C => (A, B)): JsonEncoder[C] = self.zip(that).contramap(f)
}

object JsonEncoder extends GeneratedTupleEncoders with EncoderLowPriority1 with JsonEncoderVersionSpecific {
  private class FastStringWritePool {
    private[this] var weakRef: java.lang.ref.WeakReference[Array[FastStringWrite]] =
      new java.lang.ref.WeakReference(Array(new FastStringWrite(64)))
    private[this] var level: Int = 0

    def acquire(): FastStringWrite = {
      var writes = weakRef.get
      var level  = this.level
      if (writes eq null) { // the reference was collected by GC
        level = 0
        writes = new Array(0)
      }
      if (level == writes.length) { // exceding the deepest level of recusion
        writes = java.util.Arrays.copyOf(writes, level + 1)
        writes(level) = new FastStringWrite(64)
        weakRef = new java.lang.ref.WeakReference(writes)
      }
      val write = writes(level)
      this.level = level + 1 // increase the level of recusrion
      write.reset()
      write
    }

    def release(): Unit = level -= 1 // decrease the level of recusrion
  }

  private val writePools = new ThreadLocal[FastStringWritePool] {
    override def initialValue(): FastStringWritePool = new FastStringWritePool
  }

  @inline def apply[A](implicit a: JsonEncoder[A]): JsonEncoder[A] = a

  implicit val string: JsonEncoder[String] = new JsonEncoder[String] {
    override def unsafeEncode(a: String, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      val len = a.length
      var i   = 0
      while (i < len) {
        val c = a.charAt(i)
        i += 1
        if (c == '"' || c == '\\' || c < ' ') {
          writeEncoded(a, out)
          return
        }
      }
      out.write(a)
      out.write('"')
    }

    override final def toJsonAST(a: String): Either[String, Json] = new Right(new Json.Str(a))

    private[this] def writeEncoded(a: String, out: Write): Unit = {
      val len = a.length
      var i   = 0
      while (i < len) {
        (a.charAt(i): @switch) match {
          case '"'  => out.write('\\', '"')
          case '\\' => out.write('\\', '\\')
          case '\b' => out.write('\\', 'b')
          case '\f' => out.write('\\', 'f')
          case '\n' => out.write('\\', 'n')
          case '\r' => out.write('\\', 'r')
          case '\t' => out.write('\\', 't')
          case c =>
            if (c >= ' ') out.write(c)
            else {
              out.write('\\', 'u')
              SafeNumbers.writeHex(c, out)
            }
        }
        i += 1
      }
      out.write('"')
    }
  }

  implicit val char: JsonEncoder[Char] = new JsonEncoder[Char] {
    override def unsafeEncode(a: Char, indent: Option[Int], out: Write): Unit =
      (a: @switch) match {
        case '"'  => out.write('"', '\\', '"', '"')
        case '\\' => out.write('"', '\\', '\\', '"')
        case '\b' => out.write('"', '\\', 'b', '"')
        case '\f' => out.write('"', '\\', 'f', '"')
        case '\n' => out.write('"', '\\', 'n', '"')
        case '\r' => out.write('"', '\\', 'r', '"')
        case '\t' => out.write('"', '\\', 't', '"')
        case c =>
          if (c >= ' ') out.write('"', c, '"')
          else {
            out.write('"', '\\', 'u')
            SafeNumbers.writeHex(c, out)
            out.write('"')
          }
      }

    override final def toJsonAST(a: Char): Either[String, Json] = new Right(new Json.Str(a.toString))
  }

  // FIXME: remove in the next major version
  private[json] def explicit[A](f: A => String, g: A => Json): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write(f(a))

    override final def toJsonAST(a: A): Either[String, Json] = new Right(g(a))
  }

  // FIXME: remove in the next major version
  private[json] def stringify[A](f: A => String): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(f(a))
      out.write('"')
    }

    override final def toJsonAST(a: A): Either[String, Json] = new Right(new Json.Str(f(a)))
  }

  // FIXME: add tests
  def suspend[A](encoder0: => JsonEncoder[A]): JsonEncoder[A] =
    new JsonEncoder[A] {
      lazy val encoder = encoder0

      override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = encoder.unsafeEncode(a, indent, out)

      override def isNothing(a: A): Boolean = encoder.isNothing(a)

      override def isEmpty(a: A): Boolean = encoder.isEmpty(a)

      override def toJsonAST(a: A): Either[String, Json] = encoder.toJsonAST(a)
    }

  implicit val boolean: JsonEncoder[Boolean] = new JsonEncoder[Boolean] {
    def unsafeEncode(a: Boolean, indent: Option[Int], out: Write): Unit =
      if (a) out.write('t', 'r', 'u', 'e')
      else out.write('f', 'a', 'l', 's', 'e')

    override final def toJsonAST(a: Boolean): Either[String, Json] = new Right(Json.Bool(a))
  }
  implicit val symbol: JsonEncoder[Symbol] = string.contramap(_.name)
  implicit val byte: JsonEncoder[Byte] = new JsonEncoder[Byte] {
    def unsafeEncode(a: Byte, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.toInt, out)

    override def toJsonAST(a: Byte): Either[String, Json] = new Right(Json.Num(a.toInt))
  }
  implicit val short: JsonEncoder[Short] = new JsonEncoder[Short] {
    def unsafeEncode(a: Short, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.toInt, out)

    override def toJsonAST(a: Short): Either[String, Json] = new Right(Json.Num(a.toInt))
  }
  implicit val int: JsonEncoder[Int] = new JsonEncoder[Int] {
    def unsafeEncode(a: Int, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Int): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val long: JsonEncoder[Long] = new JsonEncoder[Long] {
    def unsafeEncode(a: Long, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Long): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val bigInteger: JsonEncoder[java.math.BigInteger] = new JsonEncoder[java.math.BigInteger] {
    def unsafeEncode(a: java.math.BigInteger, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: java.math.BigInteger): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val scalaBigInt: JsonEncoder[BigInt] = new JsonEncoder[BigInt] {
    def unsafeEncode(a: BigInt, indent: Option[Int], out: Write): Unit =
      if (a.isValidLong) SafeNumbers.write(a.longValue, out)
      else SafeNumbers.write(a.bigInteger, out)

    override def toJsonAST(a: BigInt): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val double: JsonEncoder[Double] = new JsonEncoder[Double] {
    def unsafeEncode(a: Double, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Double): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val float: JsonEncoder[Float] = new JsonEncoder[Float] {
    def unsafeEncode(a: Float, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Float): Either[String, Json] = new Right(Json.Num(a))
  }
  implicit val bigDecimal: JsonEncoder[java.math.BigDecimal] = new JsonEncoder[java.math.BigDecimal] {
    def unsafeEncode(a: java.math.BigDecimal, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: java.math.BigDecimal): Either[String, Json] = new Right(new Json.Num(a))
  }
  implicit val scalaBigDecimal: JsonEncoder[BigDecimal] = new JsonEncoder[BigDecimal] {
    def unsafeEncode(a: BigDecimal, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.bigDecimal, out)

    override def toJsonAST(a: BigDecimal): Either[String, Json] = new Right(new Json.Num(a.bigDecimal))
  }

  implicit def option[A](implicit A: JsonEncoder[A]): JsonEncoder[Option[A]] = new JsonEncoder[Option[A]] {
    def unsafeEncode(oa: Option[A], indent: Option[Int], out: Write): Unit =
      if (oa eq None) out.write('n', 'u', 'l', 'l')
      else A.unsafeEncode(oa.get, indent, out)

    override def isNothing(oa: Option[A]): Boolean = (oa eq None) || A.isNothing(oa.get)

    override final def toJsonAST(oa: Option[A]): Either[String, Json] =
      if (oa eq None) new Right(Json.Null)
      else A.toJsonAST(oa.get)
  }

  def bump(indent: Option[Int]): Option[Int] =
    if (indent ne None) new Some(indent.get + 1)
    else indent

  def pad(indent: Option[Int], out: Write): Unit =
    if (indent ne None) {
      out.write('\n')
      var i  = indent.get
      val ws = 8224: Short
      while (i > 4) {
        out.write(ws, ws, ws, ws)
        i -= 4
      }
      while (i > 0) {
        out.write(ws)
        i -= 1
      }
    }

  implicit def either[A, B](implicit A: JsonEncoder[A], B: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    new JsonEncoder[Either[A, B]] {
      def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: Write): Unit = {
        out.write('{')
        if (indent.isDefined) unsafeEncodePadded(eab, indent, out)
        else unsafeEncodeCompact(eab, indent, out)
        out.write('}')
      }

      private[this] def unsafeEncodeCompact(eab: Either[A, B], indent: Option[Int], out: Write): Unit =
        eab match {
          case Left(a) =>
            out.write("\"Left\":")
            A.unsafeEncode(a, indent, out)
          case Right(b) =>
            out.write("\"Right\":")
            B.unsafeEncode(b, indent, out)
        }

      private[this] def unsafeEncodePadded(eab: Either[A, B], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        eab match {
          case Left(a) =>
            out.write("\"Left\" : ")
            A.unsafeEncode(a, indent_, out)
          case Right(b) =>
            out.write("\"Right\" : ")
            B.unsafeEncode(b, indent_, out)
        }
        pad(indent, out)
      }

      override final def toJsonAST(eab: Either[A, B]): Either[String, Json] =
        eab match {
          case Left(a)  => A.toJsonAST(a).map(v => Json.Obj(Chunk.single("Left" -> v)))
          case Right(b) => B.toJsonAST(b).map(v => Json.Obj(Chunk.single("Right" -> v)))
        }
    }

  def orElseEither[A, B](implicit A: JsonEncoder[A], B: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    new JsonEncoder[Either[A, B]] {
      def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: Write): Unit =
        eab match {
          case Left(a)  => A.unsafeEncode(a, indent, out)
          case Right(b) => B.unsafeEncode(b, indent, out)
        }
    }

}

private[json] trait EncoderLowPriority1 extends EncoderLowPriority2 {
  this: JsonEncoder.type =>

  implicit def array[A](implicit A: JsonEncoder[A], classTag: scala.reflect.ClassTag[A]): JsonEncoder[Array[A]] =
    new JsonEncoder[Array[A]] {
      override def isEmpty(as: Array[A]): Boolean = as.isEmpty

      def unsafeEncode(as: Array[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write('[', ']')
        else {
          out.write('[')
          if (indent.isDefined) unsafeEncodePadded(as, indent, out)
          else unsafeEncodeCompact(as, indent, out)
          out.write(']')
        }

      private[this] def unsafeEncodeCompact(as: Array[A], indent: Option[Int], out: Write): Unit = {
        val len = as.length
        var i   = 0
        while (i < len) {
          if (i != 0) out.write(',')
          A.unsafeEncode(as(i), indent, out)
          i += 1
        }
      }

      private[this] def unsafeEncodePadded(as: Array[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        val len = as.length
        var i   = 0
        while (i < len) {
          if (i != 0) {
            out.write(',')
            pad(indent_, out)
          }
          A.unsafeEncode(as(i), indent_, out)
          i += 1
        }
        pad(indent, out)
      }

      override final def toJsonAST(as: Array[A]): Either[String, Json] =
        as.map(A.toJsonAST)
          .foldLeft[Either[String, Chunk[Json]]](Right(Chunk.empty)) { (s, i) =>
            s.flatMap(chunk => i.map(item => chunk :+ item))
          }
          .map(Json.Arr(_))
    }

  implicit def seq[A: JsonEncoder]: JsonEncoder[Seq[A]] = iterable[A, Seq]

  implicit def chunk[A: JsonEncoder]: JsonEncoder[Chunk[A]] = iterable[A, Chunk]

  implicit def nonEmptyChunk[A: JsonEncoder]: JsonEncoder[NonEmptyChunk[A]] = chunk[A].contramap(_.toChunk)

  implicit def indexedSeq[A: JsonEncoder]: JsonEncoder[IndexedSeq[A]] = iterable[A, IndexedSeq]

  implicit def linearSeq[A: JsonEncoder]: JsonEncoder[immutable.LinearSeq[A]] = iterable[A, immutable.LinearSeq]

  implicit def listSet[A: JsonEncoder]: JsonEncoder[immutable.ListSet[A]] = iterable[A, immutable.ListSet]

  implicit def treeSet[A: JsonEncoder]: JsonEncoder[immutable.TreeSet[A]] = iterable[A, immutable.TreeSet]

  implicit def list[A](implicit A: JsonEncoder[A]): JsonEncoder[List[A]] =
    new JsonEncoder[List[A]] {
      override def isEmpty(as: List[A]): Boolean = as eq Nil

      def unsafeEncode(as: List[A], indent: Option[Int], out: Write): Unit =
        if (as eq Nil) out.write('[', ']')
        else {
          out.write('[')
          if (indent.isDefined) unsafeEncodePadded(as, indent, out)
          else unsafeEncodeCompact(as, indent, out)
          out.write(']')
        }

      private[this] def unsafeEncodeCompact(as: List[A], indent: Option[Int], out: Write): Unit = {
        var as_   = as
        var first = true
        while (as_ ne Nil) {
          if (first) first = false
          else out.write(',')
          A.unsafeEncode(as_.head, indent, out)
          as_ = as_.tail
        }
      }

      private[this] def unsafeEncodePadded(as: List[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        var as_   = as
        var first = true
        while (as_ ne Nil) {
          if (first) first = false
          else {
            out.write(',')
            pad(indent_, out)
          }
          A.unsafeEncode(as_.head, indent_, out)
          as_ = as_.tail
        }
        pad(indent, out)
      }

      override final def toJsonAST(as: List[A]): Either[String, Json] =
        as.map(A.toJsonAST)
          .foldLeft[Either[String, Chunk[Json]]](Right(Chunk.empty)) { (s, i) =>
            s.flatMap(chunk => i.map(item => chunk :+ item))
          }
          .map(Json.Arr(_))
    }

  implicit def vector[A: JsonEncoder]: JsonEncoder[Vector[A]] = iterable[A, Vector]

  implicit def set[A: JsonEncoder]: JsonEncoder[Set[A]] = iterable[A, Set]

  implicit def hashSet[A: JsonEncoder]: JsonEncoder[immutable.HashSet[A]] = iterable[A, immutable.HashSet]

  implicit def sortedSet[A: Ordering: JsonEncoder]: JsonEncoder[immutable.SortedSet[A]] =
    iterable[A, immutable.SortedSet]

  implicit def map[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[Map[K, V]] = keyValueIterable[K, V, Map]

  implicit def hashMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.HashMap[K, V]] =
    keyValueIterable[K, V, immutable.HashMap]

  implicit def mutableMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[mutable.Map[K, V]] =
    keyValueIterable[K, V, mutable.Map]

  implicit def sortedMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[collection.SortedMap[K, V]] =
    keyValueIterable[K, V, collection.SortedMap]

  implicit def listMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.ListMap[K, V]] =
    keyValueIterable[K, V, immutable.ListMap]
}

private[json] trait EncoderLowPriority2 extends EncoderLowPriority3 {
  this: JsonEncoder.type =>

  implicit def iterable[A, T[X] <: Iterable[X]](implicit A: JsonEncoder[A]): JsonEncoder[T[A]] =
    new JsonEncoder[T[A]] {
      override def isEmpty(as: T[A]): Boolean = as.isEmpty

      def unsafeEncode(as: T[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write('[', ']')
        else {
          out.write('[')
          if (indent.isDefined) unsafeEncodePadded(as, indent, out)
          else unsafeEncodeCompact(as, indent, out)
          out.write(']')
        }

      private[this] def unsafeEncodeCompact(as: T[A], indent: Option[Int], out: Write): Unit =
        as.foreach {
          var first = true
          a =>
            if (first) first = false
            else out.write(',')
            A.unsafeEncode(a, indent, out)
        }

      private[this] def unsafeEncodePadded(as: T[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        as.foreach {
          var first = true
          a =>
            if (first) first = false
            else {
              out.write(',')
              pad(indent_, out)
            }
            A.unsafeEncode(a, indent_, out)
        }
        pad(indent, out)
      }

      override final def toJsonAST(as: T[A]): Either[String, Json] =
        as.map(A.toJsonAST)
          .foldLeft[Either[String, Chunk[Json]]](Right(Chunk.empty)) { (s, i) =>
            s.flatMap(chunk => i.map(item => chunk :+ item))
          }
          .map(Json.Arr(_))
    }

  // not implicit because this overlaps with encoders for lists of tuples
  def keyValueIterable[K, A, T[X, Y] <: Iterable[(X, Y)]](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[T[K, A]] = new JsonEncoder[T[K, A]] {
    override def isEmpty(a: T[K, A]): Boolean = a.isEmpty

    def unsafeEncode(kvs: T[K, A], indent: Option[Int], out: Write): Unit =
      if (kvs.isEmpty) out.write('{', '}')
      else {
        out.write('{')
        if (indent.isDefined) unsafeEncodePadded(kvs, indent, out)
        else unsafeEncodeCompact(kvs, indent, out)
        out.write('}')
      }

    private[this] def unsafeEncodeCompact(kvs: T[K, A], indent: Option[Int], out: Write): Unit =
      kvs.foreach {
        var first = true
        kv =>
          if (!A.isNothing(kv._2)) {
            if (first) first = false
            else out.write(',')
            string.unsafeEncode(K.unsafeEncodeField(kv._1), indent, out)
            out.write(':')
            A.unsafeEncode(kv._2, indent, out)
          }
      }

    private[this] def unsafeEncodePadded(kvs: T[K, A], indent: Option[Int], out: Write): Unit = {
      val indent_ = bump(indent)
      pad(indent_, out)
      kvs.foreach {
        var first = true
        kv =>
          if (!A.isNothing(kv._2)) {
            if (first) first = false
            else {
              out.write(',')
              pad(indent_, out)
            }
            string.unsafeEncode(K.unsafeEncodeField(kv._1), indent_, out)
            out.write(" : ")
            A.unsafeEncode(kv._2, indent_, out)
          }
      }
      pad(indent, out)
    }

    override final def toJsonAST(kvs: T[K, A]): Either[String, Json] =
      kvs
        .foldLeft[Either[String, Chunk[(String, Json)]]](Right(Chunk.empty)) { case (s, (k, v)) =>
          for {
            chunk <- s
            key    = K.unsafeEncodeField(k)
            value <- A.toJsonAST(v)
          } yield if (value == Json.Null) chunk else chunk :+ (key -> value)
        }
        .map(Json.Obj(_))
  }

  // not implicit because this overlaps with encoders for lists of tuples
  def keyValueChunk[K, A](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda[K, A]] =
    keyValueIterable[K, A, ({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda]
}

private[json] trait EncoderLowPriority3 extends EncoderLowPriority4 {
  this: JsonEncoder.type =>

  import java.time._

  implicit val dayOfWeek: JsonEncoder[DayOfWeek] = new JsonEncoder[DayOfWeek] {
    def unsafeEncode(a: DayOfWeek, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override final def toJsonAST(a: DayOfWeek): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }

  implicit val duration: JsonEncoder[Duration] = new JsonEncoder[Duration] {
    def unsafeEncode(a: Duration, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: Duration): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val instant: JsonEncoder[Instant] = new JsonEncoder[Instant] {
    def unsafeEncode(a: Instant, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: Instant): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val localDate: JsonEncoder[LocalDate] = new JsonEncoder[LocalDate] {
    def unsafeEncode(a: LocalDate, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: LocalDate): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val localDateTime: JsonEncoder[LocalDateTime] = new JsonEncoder[LocalDateTime] {
    def unsafeEncode(a: LocalDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: LocalDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val localTime: JsonEncoder[LocalTime] = new JsonEncoder[LocalTime] {
    def unsafeEncode(a: LocalTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: LocalTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val month: JsonEncoder[Month] = new JsonEncoder[Month] {
    def unsafeEncode(a: Month, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override final def toJsonAST(a: Month): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }

  implicit val monthDay: JsonEncoder[MonthDay] = new JsonEncoder[MonthDay] {
    def unsafeEncode(a: MonthDay, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: MonthDay): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val offsetDateTime: JsonEncoder[OffsetDateTime] = new JsonEncoder[OffsetDateTime] {
    def unsafeEncode(a: OffsetDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: OffsetDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val offsetTime: JsonEncoder[OffsetTime] = new JsonEncoder[OffsetTime] {
    def unsafeEncode(a: OffsetTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: OffsetTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val period: JsonEncoder[Period] = new JsonEncoder[Period] {
    def unsafeEncode(a: Period, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: Period): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val year: JsonEncoder[Year] = new JsonEncoder[Year] {
    def unsafeEncode(a: Year, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: Year): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val yearMonth: JsonEncoder[YearMonth] = new JsonEncoder[YearMonth] {
    def unsafeEncode(a: YearMonth, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: YearMonth): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val zonedDateTime: JsonEncoder[ZonedDateTime] = new JsonEncoder[ZonedDateTime] {
    def unsafeEncode(a: ZonedDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: ZonedDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val zoneId: JsonEncoder[ZoneId] = new JsonEncoder[ZoneId] {
    def unsafeEncode(a: ZoneId, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.getId)
      out.write('"')
    }

    override final def toJsonAST(a: ZoneId): Either[String, Json] =
      new Right(new Json.Str(a.getId))
  }

  implicit val zoneOffset: JsonEncoder[ZoneOffset] = new JsonEncoder[ZoneOffset] {
    def unsafeEncode(a: ZoneOffset, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: ZoneOffset): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  implicit val uuid: JsonEncoder[UUID] = new JsonEncoder[UUID] {
    def unsafeEncode(a: UUID, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      SafeNumbers.write(a, out)
      out.write('"')
    }

    override final def toJsonAST(a: UUID): Either[String, Json] =
      new Right(new Json.Str(SafeNumbers.toString(a)))
  }

  implicit val currency: JsonEncoder[java.util.Currency] = new JsonEncoder[java.util.Currency] {
    def unsafeEncode(a: java.util.Currency, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override final def toJsonAST(a: java.util.Currency): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }
}

private[json] trait EncoderLowPriority4 extends EncoderLowPriorityVersionSpecific {
  implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonEncoder[A] = codec.encoder
}
