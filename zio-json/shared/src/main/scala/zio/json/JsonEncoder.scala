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
import scala.reflect.ClassTag

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

    override final def toJsonAST(b: B): Either[String, Json] =
      self.toJsonAST(f(b))
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
    val writer = new FastStringWrite(64)
    unsafeEncode(a, indent, writer)
    writer.buffer
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
  def apply[A](implicit a: JsonEncoder[A]): JsonEncoder[A] = a

  implicit val string: JsonEncoder[String] = new JsonEncoder[String] {

    override def unsafeEncode(a: String, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      var i   = 0
      val len = a.length
      while (i < len) {
        (a.charAt(i): @switch) match {
          case '"'  => out.write("\\\"")
          case '\\' => out.write("\\\\")
          case '\b' => out.write("\\b")
          case '\f' => out.write("\\f")
          case '\n' => out.write("\\n")
          case '\r' => out.write("\\r")
          case '\t' => out.write("\\t")
          case c =>
            if (c < ' ') out.write("\\u%04x".format(c.toInt))
            else out.write(c)
        }
        i += 1
      }
      out.write('"')
    }

    override final def toJsonAST(a: String): Either[String, Json] =
      Right(Json.Str(a))
  }

  implicit val char: JsonEncoder[Char] = new JsonEncoder[Char] {

    override def unsafeEncode(a: Char, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      (a: @switch) match {
        case '"'  => out.write("\\\"")
        case '\\' => out.write("\\\\")
        case c =>
          if (c < ' ') out.write("\\u%04x".format(c.toInt))
          else out.write(c)
      }
      out.write('"')
    }

    override final def toJsonAST(a: Char): Either[String, Json] =
      Right(Json.Str(a.toString))
  }

  private[json] def explicit[A](f: A => String, g: A => Json): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write(f(a))

    override final def toJsonAST(a: A): Either[String, Json] =
      Right(g(a))
  }

  private[json] def stringify[A](f: A => String): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(f(a))
      out.write('"')
    }

    override final def toJsonAST(a: A): Either[String, Json] =
      Right(Json.Str(f(a)))
  }

  def suspend[A](encoder0: => JsonEncoder[A]): JsonEncoder[A] =
    new JsonEncoder[A] {
      lazy val encoder = encoder0

      override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = encoder.unsafeEncode(a, indent, out)

      override def isNothing(a: A): Boolean = encoder.isNothing(a)

      override def isEmpty(a: A): Boolean = encoder.isEmpty(a)

      override def toJsonAST(a: A): Either[String, Json] = encoder.toJsonAST(a)
    }

  implicit val boolean: JsonEncoder[Boolean] = explicit(_.toString, Json.Bool.apply)
  implicit val symbol: JsonEncoder[Symbol]   = string.contramap(_.name)
  implicit val byte: JsonEncoder[Byte]       = explicit(_.toString, n => Json.Num(n))
  implicit val short: JsonEncoder[Short]     = explicit(_.toString, n => Json.Num(n))
  implicit val int: JsonEncoder[Int]         = explicit(_.toString, n => Json.Num(n))
  implicit val long: JsonEncoder[Long]       = explicit(_.toString, n => Json.Num(n))
  implicit val bigInteger: JsonEncoder[java.math.BigInteger] =
    explicit(_.toString, n => Json.Num(new java.math.BigDecimal(n)))
  implicit val scalaBigInt: JsonEncoder[BigInt] =
    explicit(_.toString, n => Json.Num(new java.math.BigDecimal(n.bigInteger)))
  implicit val double: JsonEncoder[Double] =
    explicit(SafeNumbers.toString, n => Json.Num(n))
  implicit val float: JsonEncoder[Float] =
    explicit(SafeNumbers.toString, n => Json.Num(n))
  implicit val bigDecimal: JsonEncoder[java.math.BigDecimal] = explicit(_.toString, Json.Num.apply)
  implicit val scalaBigDecimal: JsonEncoder[BigDecimal]      = explicit(_.toString, n => Json.Num(n.bigDecimal))

  implicit def option[A](implicit A: JsonEncoder[A]): JsonEncoder[Option[A]] = new JsonEncoder[Option[A]] {

    def unsafeEncode(oa: Option[A], indent: Option[Int], out: Write): Unit = oa match {
      case None    => out.write("null")
      case Some(a) => A.unsafeEncode(a, indent, out)
    }

    override def isNothing(oa: Option[A]): Boolean =
      oa match {
        case None    => true
        case Some(a) => A.isNothing(a)
      }

    override final def toJsonAST(oa: Option[A]): Either[String, Json] =
      oa match {
        case None    => Right(Json.Null)
        case Some(a) => A.toJsonAST(a)
      }
  }

  def bump(indent: Option[Int]): Option[Int] = indent match {
    case None    => None
    case Some(i) => Some(i + 1)
  }

  def pad(indent: Option[Int], out: Write): Unit = indent match {
    case None => ()
    case Some(n) =>
      out.write('\n')
      var i = n
      while (i > 0) {
        out.write("  ")
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

  implicit def array[A](implicit
    A: JsonEncoder[A],
    classTag: ClassTag[A]
  ): JsonEncoder[Array[A]] =
    new JsonEncoder[Array[A]] {

      override def isEmpty(as: Array[A]): Boolean = as.isEmpty

      def unsafeEncode(as: Array[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write("[]")
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

  implicit def list[A: JsonEncoder]: JsonEncoder[List[A]] = iterable[A, List]

  implicit def vector[A: JsonEncoder]: JsonEncoder[Vector[A]] = iterable[A, Vector]

  implicit def set[A: JsonEncoder]: JsonEncoder[Set[A]] =
    iterable[A, Set]

  implicit def hashSet[A: JsonEncoder]: JsonEncoder[immutable.HashSet[A]] =
    iterable[A, immutable.HashSet]

  implicit def sortedSet[A: Ordering: JsonEncoder]: JsonEncoder[immutable.SortedSet[A]] =
    iterable[A, immutable.SortedSet]

  implicit def map[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[Map[K, V]] =
    keyValueIterable[K, V, Map]

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

  implicit def iterable[A, T[X] <: Iterable[X]](implicit
    A: JsonEncoder[A]
  ): JsonEncoder[T[A]] =
    new JsonEncoder[T[A]] {

      override def isEmpty(as: T[A]): Boolean = as.isEmpty

      def unsafeEncode(as: T[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write("[]")
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
      if (kvs.isEmpty) out.write("{}")
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

  implicit val dayOfWeek: JsonEncoder[DayOfWeek]           = stringify(_.toString)
  implicit val duration: JsonEncoder[Duration]             = stringify(serializers.toString)
  implicit val instant: JsonEncoder[Instant]               = stringify(serializers.toString)
  implicit val localDate: JsonEncoder[LocalDate]           = stringify(serializers.toString)
  implicit val localDateTime: JsonEncoder[LocalDateTime]   = stringify(serializers.toString)
  implicit val localTime: JsonEncoder[LocalTime]           = stringify(serializers.toString)
  implicit val month: JsonEncoder[Month]                   = stringify(_.toString)
  implicit val monthDay: JsonEncoder[MonthDay]             = stringify(serializers.toString)
  implicit val offsetDateTime: JsonEncoder[OffsetDateTime] = stringify(serializers.toString)
  implicit val offsetTime: JsonEncoder[OffsetTime]         = stringify(serializers.toString)
  implicit val period: JsonEncoder[Period]                 = stringify(serializers.toString)
  implicit val year: JsonEncoder[Year]                     = stringify(serializers.toString)
  implicit val yearMonth: JsonEncoder[YearMonth]           = stringify(serializers.toString)
  implicit val zonedDateTime: JsonEncoder[ZonedDateTime]   = stringify(serializers.toString)
  implicit val zoneId: JsonEncoder[ZoneId]                 = stringify(serializers.toString)
  implicit val zoneOffset: JsonEncoder[ZoneOffset]         = stringify(serializers.toString)

  implicit val uuid: JsonEncoder[UUID] = stringify(_.toString)

  implicit val currency: JsonEncoder[java.util.Currency] = stringify(_.toString)
}

private[json] trait EncoderLowPriority4 extends EncoderLowPriorityVersionSpecific {
  implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonEncoder[A] = codec.encoder
}
