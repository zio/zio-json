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
import zio.json.ast.JsonType.rightNull
import zio.json.internal.{ FastStringWrite, SafeNumbers, Write }
import zio.json.javatime.serializers
import zio.{ Chunk, NonEmptyChunk }
import java.util.UUID
import scala.annotation._
import scala.collection.{ immutable, mutable }

/**
 * A type class that writes values of type `A` as JSON text. The companion object has instances for the standard types,
 * `DeriveJsonEncoder.gen` derives one for a case class or a sealed trait, and [[JsonEncoder#contramap]] builds one from
 * an existing encoder. Most code does not call it directly: the `toJson` and `toJsonPretty` extension methods from
 * `import zio.json._` find the encoder implicitly.
 *
 * {{{
 * final case class Book(title: String, pages: Int)
 *
 * object Book {
 *   implicit val encoder: JsonEncoder[Book] = DeriveJsonEncoder.gen[Book]
 * }
 *
 * val json: String = Book("Dune", 412).toJson
 * // {"title":"Dune","pages":412}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/encoding/ Encoding]]
 */
trait JsonEncoder[A] extends JsonEncoderPlatformSpecific[A] {
  self =>

  /**
   * Returns an encoder for `B` that converts each value to `A` with `f` and writes the result with this encoder. The
   * new encoder's `isNothing`, `isEmpty` and `toJsonAST` also go through `f`.
   *
   * {{{
   * final case class OrderId(value: Long)
   *
   * val orderIdEncoder: JsonEncoder[OrderId] = JsonEncoder[Long].contramap(_.value)
   *
   * val json = orderIdEncoder.encodeJson(OrderId(1042L)).toString
   * // 1042
   * }}}
   */
  final def contramap[B](f: B => A): JsonEncoder[B] = new JsonEncoder.AbstractJsonEncoder[B] {
    override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
      self.unsafeEncode(f(b), indent, out)

    override def isNothing(b: B): Boolean = self.isNothing(f(b))

    override def isEmpty(b: B): Boolean = self.isEmpty(f(b))

    override def toJsonAST(b: B): Either[String, Json] = self.toJsonAST(f(b))
  }

  /**
   * Returns an encoder for `Either[A, B]` that writes a `Left` with this encoder and a `Right` with `that`, wrapped in
   * an object with a single field, `Left` or `Right`. It writes the same JSON as the implicit `Either` encoder.
   *
   * {{{
   * val encoder: JsonEncoder[Either[Int, String]] = JsonEncoder[Int].either(JsonEncoder[String])
   *
   * val left  = encoder.encodeJson(Left(3)).toString          // {"Left":3}
   * val right = encoder.encodeJson(Right("sold out")).toString // {"Right":"sold out"}
   * }}}
   */
  final def either[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]] = JsonEncoder.either[A, B](self, that)

  /**
   * Returns an encoder for `Either[A, B]` that writes a `Left` with this encoder and a `Right` with `that`, with no
   * `Left` or `Right` field around the value: `Right("John Doe")` is written as `"John Doe"`, not as
   * `{"Right":"John Doe"}`. The JSON does not record which side the value came from.
   *
   * {{{
   * val encoder: JsonEncoder[Either[Int, String]] = JsonEncoder[Int].orElseEither(JsonEncoder[String])
   *
   * val left  = encoder.encodeJson(Left(3)).toString          // 3
   * val right = encoder.encodeJson(Right("John Doe")).toString // "John Doe"
   * }}}
   */
  final def orElseEither[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    JsonEncoder.orElseEither[A, B](self, that)

  /**
   * Returns an encoder for `C` that splits each value into an `Either[A, B]` with `f` and writes it like
   * [[JsonEncoder#either]]: a `Left` as `{"Left":...}` with this encoder, a `Right` as `{"Right":...}` with `that`.
   */
  final def eitherWith[B, C](that: => JsonEncoder[B])(f: C => Either[A, B]): JsonEncoder[C] =
    self.either(that).contramap(f)

  /**
   * Encodes `a` as JSON text. With `indent = None` (the default) the output is compact. With `Some(n)` it is pretty
   * printed with two spaces per nesting level, starting at level `n`, and object fields are written as
   * `"name" : value`; `toJsonPretty` uses `Some(0)`.
   *
   * {{{
   * final case class Sensor(id: String, celsius: Double)
   *
   * implicit val sensorEncoder: JsonEncoder[Sensor] = DeriveJsonEncoder.gen[Sensor]
   *
   * val compact = JsonEncoder[Sensor].encodeJson(Sensor("s-7", 21.5)).toString
   * // {"id":"s-7","celsius":21.5}
   * }}}
   *
   * @return
   *   the JSON text; call `toString` on it for a `String`
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
   * Returns `true` when `a` counts as a missing value. A derived encoder leaves out a field whose value is missing,
   * unless the `explicitNulls` setting asks for `null`, and the map encoders drop such entries. The default returns
   * `false`; the `Option` encoder returns `true` for `None` and for `Some(None)`.
   */
  def isNothing(a: A): Boolean = false

  /**
   * Returns `true` when `a` is an empty collection. A derived encoder leaves out a field whose value is empty when the
   * `explicitEmptyCollections` setting turns empty collections off. The default returns `false`; the collection and map
   * encoders return `true` for an empty value.
   */
  def isEmpty(a: A): Boolean = false

  /**
   * Returns this encoder typed as an encoder for the subtype `B`. It is a cast: no new encoder is created.
   */
  final def narrow[B <: A]: JsonEncoder[B] = self.asInstanceOf[JsonEncoder[B]]

  /**
   * Writes the JSON for `a` to `out`. It is the only abstract member of `JsonEncoder`; [[JsonEncoder#encodeJson]] and
   * the `toJson` and `toJsonPretty` extension methods call it. `indent` is `None` for compact output and `Some(level)`
   * for pretty output, where `level` is the nesting level of `a`; `JsonEncoder.bump` and `JsonEncoder.pad` help with
   * indentation. Implement it when you write an encoder by hand; to encode a value, call `encodeJson` or `toJson`.
   *
   * `Write` is in the `zio.json.internal` package.
   */
  def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit

  /**
   * Encodes `a` as a [[zio.json.ast.Json]] tree. The default implementation encodes `a` to JSON text with
   * `encodeJson(a, None)` and parses that text with `Json.decoder`. Most built-in encoders and derived encoders
   * override it to build the tree directly; the tuple encoders and `orElseEither` use the default. A hand-written
   * encoder can override it to skip the round trip through text, and should keep it consistent with `unsafeEncode`.
   *
   * {{{
   * import zio.json.ast.Json
   *
   * val ast: Either[String, Json] = JsonEncoder[List[Int]].toJsonAST(List(3, 5, 8))
   * // Right([3,5,8])
   * }}}
   */
  def toJsonAST(a: A): Either[String, Json] = Json.decoder.decodeJson(encodeJson(a, None))

  /**
   * Returns an encoder for the pair `(A, B)` that writes a two-element JSON array: the first element with this encoder,
   * the second with `that`.
   *
   * {{{
   * val encoder: JsonEncoder[(String, Int)] = JsonEncoder[String].zip(JsonEncoder[Int])
   *
   * val json = encoder.encodeJson(("apples", 3)).toString
   * // ["apples",3]
   * }}}
   */
  final def zip[B](that: => JsonEncoder[B]): JsonEncoder[(A, B)] = JsonEncoder.tuple2(self, that)

  /**
   * Returns an encoder for `C` that converts each value to a pair with `f` and writes it like [[JsonEncoder#zip]], as a
   * two-element JSON array.
   */
  final def zipWith[B, C](that: => JsonEncoder[B])(f: C => (A, B)): JsonEncoder[C] = self.zip(that).contramap(f)
}

/**
 * Built-in [[JsonEncoder]] instances and helpers for writing encoders by hand. The implicit instances cover the
 * primitive types, `Option`, `Either`, tuples of 1 to 22 elements (as JSON arrays), the standard collections and maps,
 * the `java.time` types, `UUID` and `Currency`, and are found without an import.
 */
object JsonEncoder extends GeneratedTupleEncoders with EncoderLowPriority1 with JsonEncoderVersionSpecific {

  /** Abstract class of the `JsonEncoder` trait to reduce class file size in subclasses. */
  abstract class AbstractJsonEncoder[A] extends JsonEncoder[A]
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

    def release(): Unit = if (level > 0) level -= 1 // decrease the level of recusrion
  }

  private val writePools = new ThreadLocal[FastStringWritePool] {
    override def initialValue(): FastStringWritePool = new FastStringWritePool
  }

  /**
   * Returns the implicit `JsonEncoder[A]` in scope, so `JsonEncoder[Int]` reads as a lookup.
   */
  @inline def apply[A](implicit a: JsonEncoder[A]): JsonEncoder[A] = a

  /**
   * Encodes a `String` as a JSON string. It escapes `"` and `\` with a backslash, and control characters below a space
   * as `\n`, `\t`, `\r`, `\b`, `\f` or a hex escape; every other character, including non-ASCII ones, is written as it
   * is. On the JVM, a `null` string throws a `java.lang.NullPointerException`.
   */
  implicit val string: JsonEncoder[String] = new JsonEncoder.AbstractJsonEncoder[String] {
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

    @inline override def toJsonAST(a: String): Either[String, Json] = new Right(new Json.Str(a))

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
          case c    =>
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

  /**
   * Encodes a `Char` as a JSON string of one character, escaped like the `String` encoder: `'"'` is written as `"\""`.
   */
  implicit val char: JsonEncoder[Char] = new JsonEncoder.AbstractJsonEncoder[Char] {
    override def unsafeEncode(a: Char, indent: Option[Int], out: Write): Unit =
      (a: @switch) match {
        case '"'  => out.write('"', '\\', '"', '"')
        case '\\' => out.write('"', '\\', '\\', '"')
        case '\b' => out.write('"', '\\', 'b', '"')
        case '\f' => out.write('"', '\\', 'f', '"')
        case '\n' => out.write('"', '\\', 'n', '"')
        case '\r' => out.write('"', '\\', 'r', '"')
        case '\t' => out.write('"', '\\', 't', '"')
        case c    =>
          if (c >= ' ') out.write('"', c, '"')
          else {
            out.write('"', '\\', 'u')
            SafeNumbers.writeHex(c, out)
            out.write('"')
          }
      }

    override def toJsonAST(a: Char): Either[String, Json] = new Right(new Json.Str(a.toString))
  }

  // FIXME: add tests
  /**
   * Returns an encoder that builds `encoder0` the first time it is used and then delegates `unsafeEncode`, `isNothing`,
   * `isEmpty` and `toJsonAST` to it. Use it for a recursive type, whose encoder refers to itself.
   *
   * {{{
   * final case class Category(name: String, children: List[Category])
   *
   * object Category {
   *   implicit lazy val encoder: JsonEncoder[Category] = JsonEncoder.suspend(DeriveJsonEncoder.gen[Category])
   * }
   *
   * val json = Category("books", List(Category("fiction", Nil))).toJson
   * // {"name":"books","children":[{"name":"fiction","children":[]}]}
   * }}}
   */
  def suspend[A](encoder0: => JsonEncoder[A]): JsonEncoder[A] =
    new JsonEncoder.AbstractJsonEncoder[A] {
      lazy val encoder = encoder0

      override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = encoder.unsafeEncode(a, indent, out)

      override def isNothing(a: A): Boolean = encoder.isNothing(a)

      override def isEmpty(a: A): Boolean = encoder.isEmpty(a)

      override def toJsonAST(a: A): Either[String, Json] = encoder.toJsonAST(a)
    }

  /**
   * Encodes a `Boolean` as `true` or `false`.
   */
  implicit val boolean: JsonEncoder[Boolean] = new JsonEncoder.AbstractJsonEncoder[Boolean] {
    def unsafeEncode(a: Boolean, indent: Option[Int], out: Write): Unit =
      if (a) out.write('t', 'r', 'u', 'e')
      else out.write('f', 'a', 'l', 's', 'e')

    override def toJsonAST(a: Boolean): Either[String, Json] = new Right(Json.Bool(a))
  }

  /**
   * Encodes `()` as the empty JSON object `{}`.
   */
  implicit val unit: JsonEncoder[Unit] = new JsonEncoder.AbstractJsonEncoder[Unit] {
    def unsafeEncode(a: Unit, indent: Option[Int], out: Write): Unit = out.write('{', '}')

    override def toJsonAST(a: Unit): Either[String, Json] = new Right(Json.Obj(Chunk.empty))
  }

  /**
   * Encodes a `Symbol` as a JSON string that holds its name: `Symbol("draft")` is written as `"draft"`.
   */
  implicit val symbol: JsonEncoder[Symbol] = string.contramap(_.name)

  /**
   * Encodes a `Byte` as a JSON number.
   */
  implicit val byte: JsonEncoder[Byte] = new JsonEncoder.AbstractJsonEncoder[Byte] {
    def unsafeEncode(a: Byte, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.toInt, out)

    override def toJsonAST(a: Byte): Either[String, Json] = new Right(Json.Num(a.toInt))
  }

  /**
   * Encodes a `Short` as a JSON number.
   */
  implicit val short: JsonEncoder[Short] = new JsonEncoder.AbstractJsonEncoder[Short] {
    def unsafeEncode(a: Short, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.toInt, out)

    override def toJsonAST(a: Short): Either[String, Json] = new Right(Json.Num(a.toInt))
  }

  /**
   * Encodes an `Int` as a JSON number.
   */
  implicit val int: JsonEncoder[Int] = new JsonEncoder.AbstractJsonEncoder[Int] {
    def unsafeEncode(a: Int, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Int): Either[String, Json] = new Right(Json.Num(a))
  }

  /**
   * Encodes a `Long` as a JSON number.
   */
  implicit val long: JsonEncoder[Long] = new JsonEncoder.AbstractJsonEncoder[Long] {
    def unsafeEncode(a: Long, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Long): Either[String, Json] = new Right(Json.Num(a))
  }

  /**
   * Encodes a `java.math.BigInteger` as a JSON number with all its digits.
   */
  implicit val bigInteger: JsonEncoder[java.math.BigInteger] =
    new JsonEncoder.AbstractJsonEncoder[java.math.BigInteger] {
      def unsafeEncode(a: java.math.BigInteger, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

      override def toJsonAST(a: java.math.BigInteger): Either[String, Json] = new Right(Json.Num(a))
    }

  /**
   * Encodes a `BigInt` as a JSON number with all its digits.
   */
  implicit val scalaBigInt: JsonEncoder[BigInt] = new JsonEncoder.AbstractJsonEncoder[BigInt] {
    def unsafeEncode(a: BigInt, indent: Option[Int], out: Write): Unit =
      if (a.isValidLong) SafeNumbers.write(a.longValue, out)
      else SafeNumbers.write(a.bigInteger, out)

    override def toJsonAST(a: BigInt): Either[String, Json] = new Right(Json.Num(a))
  }

  /**
   * Encodes a `Double` as a JSON number, such as `1.0`, `21.5` or `1.0E20`. `NaN`, `Infinity` and `-Infinity` are
   * written as the JSON strings `"NaN"`, `"Infinity"` and `"-Infinity"`. A known bug: `toJsonAST` throws a
   * `java.lang.NumberFormatException` for these three values instead of returning a `Left`.
   */
  implicit val double: JsonEncoder[Double] = new JsonEncoder.AbstractJsonEncoder[Double] {
    def unsafeEncode(a: Double, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Double): Either[String, Json] = new Right(Json.Num(a))
  }

  /**
   * Encodes a `Float` as a JSON number, such as `1.1` or `3.0`. `NaN`, `Infinity` and `-Infinity` are written as the
   * JSON strings `"NaN"`, `"Infinity"` and `"-Infinity"`. A known bug: `toJsonAST` throws a
   * `java.lang.NumberFormatException` for these three values instead of returning a `Left`.
   */
  implicit val float: JsonEncoder[Float] = new JsonEncoder.AbstractJsonEncoder[Float] {
    def unsafeEncode(a: Float, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

    override def toJsonAST(a: Float): Either[String, Json] = new Right(Json.Num(a))
  }

  /**
   * Encodes a `java.math.BigDecimal` as a JSON number that keeps its scale and notation: `1.50` is written as `1.50`
   * and `1E+3` as `1E+3`.
   */
  implicit val bigDecimal: JsonEncoder[java.math.BigDecimal] =
    new JsonEncoder.AbstractJsonEncoder[java.math.BigDecimal] {
      def unsafeEncode(a: java.math.BigDecimal, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a, out)

      override def toJsonAST(a: java.math.BigDecimal): Either[String, Json] = new Right(new Json.Num(a))
    }

  /**
   * Encodes a `BigDecimal` as a JSON number that keeps its scale: `BigDecimal("19.990")` is written as `19.990`.
   */
  implicit val scalaBigDecimal: JsonEncoder[BigDecimal] = new JsonEncoder.AbstractJsonEncoder[BigDecimal] {
    def unsafeEncode(a: BigDecimal, indent: Option[Int], out: Write): Unit = SafeNumbers.write(a.bigDecimal, out)

    override def toJsonAST(a: BigDecimal): Either[String, Json] = new Right(new Json.Num(a.bigDecimal))
  }

  /**
   * Encodes `None` as `null` and `Some(a)` with the encoder for `A`. Its `isNothing` is `true` for `None` and for a
   * `Some` whose content is itself missing, such as `Some(None)`, so derived encoders and the map encoders leave such
   * values out by default.
   */
  implicit def option[A](implicit A: JsonEncoder[A]): JsonEncoder[Option[A]] =
    new JsonEncoder.AbstractJsonEncoder[Option[A]] {
      def unsafeEncode(oa: Option[A], indent: Option[Int], out: Write): Unit =
        if (oa eq None) out.write('n', 'u', 'l', 'l')
        else A.unsafeEncode(oa.get, indent, out)

      override def isNothing(oa: Option[A]): Boolean = (oa eq None) || A.isNothing(oa.get)

      override def toJsonAST(oa: Option[A]): Either[String, Json] =
        if (oa eq None) rightNull
        else A.toJsonAST(oa.get)
    }

  /**
   * Returns the indent for one nesting level deeper: `Some(n + 1)` for `Some(n)`, and `None` for `None`. Hand-written
   * encoders pass the result to the encoders of nested values.
   */
  def bump(indent: Option[Int]): Option[Int] =
    if (indent ne None) new Some(indent.get + 1)
    else indent

  /**
   * Writes a newline followed by two spaces per level to `out` when `indent` is `Some(level)`, and writes nothing when
   * it is `None`. Hand-written encoders call it before each element or field and before the closing bracket, so that
   * pretty output puts one element on each line.
   */
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

  /**
   * Encodes `Left(a)` as `{"Left":a}` and `Right(b)` as `{"Right":b}`, with the encoders for `A` and `B`.
   */
  implicit def either[A, B](implicit A: JsonEncoder[A], B: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    new JsonEncoder.AbstractJsonEncoder[Either[A, B]] {
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

      override def toJsonAST(eab: Either[A, B]): Either[String, Json] =
        eab match {
          case Left(a)  => A.toJsonAST(a).map(v => Json.Obj(Chunk.single("Left" -> v)))
          case Right(b) => B.toJsonAST(b).map(v => Json.Obj(Chunk.single("Right" -> v)))
        }
    }

  /**
   * Returns an encoder for `Either[A, B]` that writes the content of a `Left` or a `Right` with no wrapper object:
   * `Left(3)` is written as `3`. It is not implicit, so the implicit `Either` encoder, which writes `{"Left":3}`, stays
   * the default.
   */
  def orElseEither[A, B](implicit A: JsonEncoder[A], B: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    new JsonEncoder.AbstractJsonEncoder[Either[A, B]] {
      def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: Write): Unit =
        eab match {
          case Left(a)  => A.unsafeEncode(a, indent, out)
          case Right(b) => B.unsafeEncode(b, indent, out)
        }
    }

}

private[json] trait EncoderLowPriority1 extends EncoderLowPriority2 {
  this: JsonEncoder.type =>

  /**
   * Encodes an `Array` as a JSON array of its elements. It needs a `ClassTag` for the element type.
   */
  implicit def array[A](implicit A: JsonEncoder[A], classTag: scala.reflect.ClassTag[A]): JsonEncoder[Array[A]] =
    new JsonEncoder.AbstractJsonEncoder[Array[A]] {
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
        val len     = as.length
        var i       = 0
        while (i < len) {
          if (i != 0) out.write(',')
          pad(indent_, out)
          A.unsafeEncode(as(i), indent_, out)
          i += 1
        }
        pad(indent, out)
      }

      override def toJsonAST(as: Array[A]): Either[String, Json] = {
        val len = as.length
        val buf = new Array[Json](len)
        var i   = 0
        while (i < len) {
          A.toJsonAST(as(i)) match {
            case Right(json) =>
              buf(i) = json
              i += 1
            case left =>
              return left
          }
        }
        new Right(Json.Arr(Chunk.fromArray(buf)))
      }
    }

  /**
   * Encodes a `Seq` as a JSON array of its elements.
   */
  implicit def seq[A: JsonEncoder]: JsonEncoder[Seq[A]] = iterable[A, Seq]

  /**
   * Encodes a `zio.Chunk` as a JSON array of its elements.
   */
  implicit def chunk[A: JsonEncoder]: JsonEncoder[Chunk[A]] = iterable[A, Chunk]

  /**
   * Encodes a `zio.NonEmptyChunk` as a JSON array of its elements.
   */
  implicit def nonEmptyChunk[A: JsonEncoder]: JsonEncoder[NonEmptyChunk[A]] = chunk[A].contramap(_.toChunk)

  /**
   * Encodes an `IndexedSeq` as a JSON array of its elements.
   */
  implicit def indexedSeq[A: JsonEncoder]: JsonEncoder[IndexedSeq[A]] = iterable[A, IndexedSeq]

  /**
   * Encodes an `immutable.LinearSeq` as a JSON array of its elements.
   */
  implicit def linearSeq[A: JsonEncoder]: JsonEncoder[immutable.LinearSeq[A]] = iterable[A, immutable.LinearSeq]

  /**
   * Encodes an `immutable.ListSet` as a JSON array of its elements, in insertion order.
   */
  implicit def listSet[A: JsonEncoder]: JsonEncoder[immutable.ListSet[A]] = iterable[A, immutable.ListSet]

  /**
   * Encodes an `immutable.TreeSet` as a JSON array of its elements, in sorted order.
   */
  implicit def treeSet[A: JsonEncoder]: JsonEncoder[immutable.TreeSet[A]] = iterable[A, immutable.TreeSet]

  /**
   * Encodes a `List` as a JSON array of its elements.
   */
  implicit def list[A](implicit A: JsonEncoder[A]): JsonEncoder[List[A]] =
    new JsonEncoder.AbstractJsonEncoder[List[A]] {
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
        var as_ = as
        while (as_ ne Nil) {
          if (as_ ne as) out.write(',')
          A.unsafeEncode(as_.head, indent, out)
          as_ = as_.tail
        }
      }

      private[this] def unsafeEncodePadded(as: List[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        var as_     = as
        while (as_ ne Nil) {
          if (as_ ne as) out.write(',')
          pad(indent_, out)
          A.unsafeEncode(as_.head, indent_, out)
          as_ = as_.tail
        }
        pad(indent, out)
      }

      override def toJsonAST(as: List[A]): Either[String, Json] = {
        var as_ = as
        val buf = new Array[Json](as_.size)
        var i   = 0
        while (as_ ne Nil) {
          A.toJsonAST(as_.head) match {
            case Right(json) =>
              as_ = as_.tail
              buf(i) = json
              i += 1
            case left =>
              return left
          }
        }
        new Right(Json.Arr(Chunk.fromArray(buf)))
      }
    }

  /**
   * Encodes a `Vector` as a JSON array of its elements.
   */
  implicit def vector[A: JsonEncoder]: JsonEncoder[Vector[A]] = iterable[A, Vector]

  /**
   * Encodes a `Set` as a JSON array of its elements, in the set's iteration order.
   */
  implicit def set[A: JsonEncoder]: JsonEncoder[Set[A]] = iterable[A, Set]

  /**
   * Encodes an `immutable.HashSet` as a JSON array of its elements, in the set's iteration order.
   */
  implicit def hashSet[A: JsonEncoder]: JsonEncoder[immutable.HashSet[A]] = iterable[A, immutable.HashSet]

  /**
   * Encodes an `immutable.SortedSet` as a JSON array of its elements, in sorted order.
   */
  implicit def sortedSet[A: Ordering: JsonEncoder]: JsonEncoder[immutable.SortedSet[A]] =
    iterable[A, immutable.SortedSet]

  /**
   * Encodes a `Map` as a JSON object, with the `JsonFieldEncoder` for the keys: `Map(1 -> "one")` is written as
   * `{"1":"one"}`. Entries whose value is missing, such as `None`, are left out. A known bug: `toJsonAST` drops the
   * entries whose value becomes `null` instead, so the two disagree for a value such as `Json.Null`, which `encodeJson`
   * keeps.
   */
  implicit def map[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[Map[K, V]] = keyValueIterable[K, V, Map]

  /**
   * Encodes an `immutable.HashMap` as a JSON object, in the same way as the `Map` encoder.
   */
  implicit def hashMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.HashMap[K, V]] =
    keyValueIterable[K, V, immutable.HashMap]

  /**
   * Encodes a `mutable.Map` as a JSON object, in the same way as the `Map` encoder.
   */
  implicit def mutableMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[mutable.Map[K, V]] =
    keyValueIterable[K, V, mutable.Map]

  /**
   * Encodes a `collection.SortedMap` as a JSON object, in the same way as the `Map` encoder, with the keys in sorted
   * order.
   */
  implicit def sortedMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[collection.SortedMap[K, V]] =
    keyValueIterable[K, V, collection.SortedMap]

  /**
   * Encodes an `immutable.ListMap` as a JSON object, in the same way as the `Map` encoder, with the keys in insertion
   * order.
   */
  implicit def listMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.ListMap[K, V]] =
    keyValueIterable[K, V, immutable.ListMap]
}

private[json] trait EncoderLowPriority2 extends EncoderLowPriority3 {
  this: JsonEncoder.type =>

  /**
   * Encodes any collection `T[A]` that is an `Iterable` as a JSON array of its elements, in iteration order. It has a
   * lower priority than the encoders for specific collections, and covers the ones without an encoder of their own,
   * such as `immutable.Queue`.
   */
  implicit def iterable[A, T[X] <: Iterable[X]](implicit A: JsonEncoder[A]): JsonEncoder[T[A]] =
    new JsonEncoder.AbstractJsonEncoder[T[A]] {
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
          var comma = false
          a =>
            if (comma) out.write(',')
            else comma = true
            A.unsafeEncode(a, indent, out)
        }

      private[this] def unsafeEncodePadded(as: T[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        as.foreach {
          var comma = false
          a =>
            if (comma) out.write(',')
            else comma = true
            pad(indent_, out)
            A.unsafeEncode(a, indent_, out)
        }
        pad(indent, out)
      }

      override def toJsonAST(as: T[A]): Either[String, Json] = {
        val it  = as.iterator
        val buf = new Array[Json](as.size)
        var i   = 0
        while (it.hasNext) {
          A.toJsonAST(it.next()) match {
            case Right(json) =>
              buf(i) = json
              i += 1
            case left =>
              return left
          }
        }
        new Right(Json.Arr(Chunk.fromArray(buf)))
      }
    }

  // not implicit because this overlaps with encoders for lists of tuples
  /**
   * Returns an encoder that writes a collection of key-value pairs as a JSON object, with the `JsonFieldEncoder` for
   * the keys. By default a sequence of pairs is written as an array of two-element arrays; this encoder is not
   * implicit, so you choose it explicitly. Every pair is written, including repeated keys, except pairs whose value is
   * missing, such as `None`. `toJsonAST` has the same known bug as the `Map` encoder.
   *
   * {{{
   * type Pairs[K, V] = List[(K, V)]
   *
   * val encoder: JsonEncoder[Pairs[String, Int]] = JsonEncoder.keyValueIterable[String, Int, Pairs]
   *
   * val json = encoder.encodeJson(List("north" -> 4, "south" -> 7)).toString
   * // {"north":4,"south":7}
   * }}}
   */
  def keyValueIterable[K, A, T[X, Y] <: Iterable[(X, Y)]](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[T[K, A]] = new JsonEncoder.AbstractJsonEncoder[T[K, A]] {
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
        var comma = false
        kv =>
          if (!A.isNothing(kv._2)) {
            if (comma) out.write(',')
            else comma = true
            string.unsafeEncode(K.unsafeEncodeField(kv._1), indent, out)
            out.write(':')
            A.unsafeEncode(kv._2, indent, out)
          }
      }

    private[this] def unsafeEncodePadded(kvs: T[K, A], indent: Option[Int], out: Write): Unit = {
      val indent_ = bump(indent)
      kvs.foreach {
        var comman = false
        kv =>
          if (!A.isNothing(kv._2)) {
            if (comman) out.write(',')
            else comman = true
            pad(indent_, out)
            string.unsafeEncode(K.unsafeEncodeField(kv._1), indent_, out)
            out.write(" : ")
            A.unsafeEncode(kv._2, indent_, out)
          }
      }
      pad(indent, out)
    }

    override def toJsonAST(kvs: T[K, A]): Either[String, Json] = {
      val it  = kvs.iterator
      var buf = new Array[(String, Json)](kvs.size)
      var i   = 0
      while (it.hasNext) {
        val kv = it.next()
        A.toJsonAST(kv._2) match {
          case Right(json) =>
            if (json ne Json.Null) {
              buf(i) = (K.unsafeEncodeField(kv._1), json)
              i += 1
            }
          case left =>
            return left
        }
      }
      if (i != buf.length) buf = java.util.Arrays.copyOf(buf, i)
      new Right(Json.Obj(Chunk.fromArray(buf)))
    }
  }

  // not implicit because this overlaps with encoders for lists of tuples
  /**
   * Returns `keyValueIterable` for a `zio.Chunk` of pairs: `Chunk("a" -> 1, "a" -> 2)` is written as `{"a":1,"a":2}`.
   * It is not implicit, so a `Chunk` of pairs is otherwise written as an array of two-element arrays.
   */
  def keyValueChunk[K, A](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda[K, A]] =
    keyValueIterable[K, A, ({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda]
}

private[json] trait EncoderLowPriority3 extends EncoderLowPriority4 {
  this: JsonEncoder.type =>

  import java.time._

  /**
   * Encodes a `java.time.DayOfWeek` as its upper-case name, such as `"MONDAY"`.
   */
  implicit val dayOfWeek: JsonEncoder[DayOfWeek] = new JsonEncoder.AbstractJsonEncoder[DayOfWeek] {
    def unsafeEncode(a: DayOfWeek, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override def toJsonAST(a: DayOfWeek): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }

  /**
   * Encodes a `java.time.Duration` as an ISO-8601 string, such as `"PT1H30M"` or `"PT0S"`.
   */
  implicit val duration: JsonEncoder[Duration] = new JsonEncoder.AbstractJsonEncoder[Duration] {
    def unsafeEncode(a: Duration, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: Duration): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.Instant` as an ISO-8601 string in UTC, such as `"2024-01-02T03:04:05.123Z"`.
   */
  implicit val instant: JsonEncoder[Instant] = new JsonEncoder.AbstractJsonEncoder[Instant] {
    def unsafeEncode(a: Instant, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: Instant): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.LocalDate` as an ISO-8601 string, such as `"2024-01-02"`.
   */
  implicit val localDate: JsonEncoder[LocalDate] = new JsonEncoder.AbstractJsonEncoder[LocalDate] {
    def unsafeEncode(a: LocalDate, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: LocalDate): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.LocalDateTime` as an ISO-8601 string that always includes the seconds, such as
   * `"2024-01-02T03:04:00"`.
   */
  implicit val localDateTime: JsonEncoder[LocalDateTime] = new JsonEncoder.AbstractJsonEncoder[LocalDateTime] {
    def unsafeEncode(a: LocalDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: LocalDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.LocalTime` as an ISO-8601 string that always includes the seconds, such as `"03:04:00"`.
   */
  implicit val localTime: JsonEncoder[LocalTime] = new JsonEncoder.AbstractJsonEncoder[LocalTime] {
    def unsafeEncode(a: LocalTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: LocalTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.Month` as its upper-case name, such as `"JANUARY"`.
   */
  implicit val month: JsonEncoder[Month] = new JsonEncoder.AbstractJsonEncoder[Month] {
    def unsafeEncode(a: Month, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override def toJsonAST(a: Month): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }

  /**
   * Encodes a `java.time.MonthDay` as an ISO-8601 string, such as `"--12-25"`.
   */
  implicit val monthDay: JsonEncoder[MonthDay] = new JsonEncoder.AbstractJsonEncoder[MonthDay] {
    def unsafeEncode(a: MonthDay, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: MonthDay): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.OffsetDateTime` as an ISO-8601 string, such as `"2024-01-02T03:04:05+02:00"`.
   */
  implicit val offsetDateTime: JsonEncoder[OffsetDateTime] = new JsonEncoder.AbstractJsonEncoder[OffsetDateTime] {
    def unsafeEncode(a: OffsetDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: OffsetDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.OffsetTime` as an ISO-8601 string, such as `"03:04:05+02:00"`.
   */
  implicit val offsetTime: JsonEncoder[OffsetTime] = new JsonEncoder.AbstractJsonEncoder[OffsetTime] {
    def unsafeEncode(a: OffsetTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: OffsetTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.Period` as an ISO-8601 string, such as `"P1Y2M3D"` or `"P0D"`.
   */
  implicit val period: JsonEncoder[Period] = new JsonEncoder.AbstractJsonEncoder[Period] {
    def unsafeEncode(a: Period, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: Period): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.Year` as a JSON string, not a number, such as `"2024"`. Years after 9999 get a `+` sign:
   * `"+12024"`.
   */
  implicit val year: JsonEncoder[Year] = new JsonEncoder.AbstractJsonEncoder[Year] {
    def unsafeEncode(a: Year, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: Year): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.YearMonth` as an ISO-8601 string, such as `"2024-01"`.
   */
  implicit val yearMonth: JsonEncoder[YearMonth] = new JsonEncoder.AbstractJsonEncoder[YearMonth] {
    def unsafeEncode(a: YearMonth, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: YearMonth): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.ZonedDateTime` as an ISO-8601 string with the offset and the zone id, such as
   * `"2024-01-02T03:04:05+01:00[Europe/Paris]"`.
   */
  implicit val zonedDateTime: JsonEncoder[ZonedDateTime] = new JsonEncoder.AbstractJsonEncoder[ZonedDateTime] {
    def unsafeEncode(a: ZonedDateTime, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: ZonedDateTime): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.time.ZoneId` as its id, such as `"Europe/Paris"` or `"+01:00"`.
   */
  implicit val zoneId: JsonEncoder[ZoneId] = new JsonEncoder.AbstractJsonEncoder[ZoneId] {
    def unsafeEncode(a: ZoneId, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.getId)
      out.write('"')
    }

    override def toJsonAST(a: ZoneId): Either[String, Json] =
      new Right(new Json.Str(a.getId))
  }

  /**
   * Encodes a `java.time.ZoneOffset` as a JSON string, such as `"+02:00"`, or `"Z"` for UTC.
   */
  implicit val zoneOffset: JsonEncoder[ZoneOffset] = new JsonEncoder.AbstractJsonEncoder[ZoneOffset] {
    def unsafeEncode(a: ZoneOffset, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      serializers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: ZoneOffset): Either[String, Json] =
      new Right(new Json.Str(serializers.toString(a)))
  }

  /**
   * Encodes a `java.util.UUID` as a JSON string in its lower-case canonical form, such as
   * `"123e4567-e89b-12d3-a456-426614174000"`.
   */
  implicit val uuid: JsonEncoder[UUID] = new JsonEncoder.AbstractJsonEncoder[UUID] {
    def unsafeEncode(a: UUID, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      SafeNumbers.write(a, out)
      out.write('"')
    }

    override def toJsonAST(a: UUID): Either[String, Json] =
      new Right(new Json.Str(SafeNumbers.toString(a)))
  }

  /**
   * Encodes a `java.util.Currency` as its ISO 4217 code, such as `"EUR"`.
   */
  implicit val currency: JsonEncoder[java.util.Currency] = new JsonEncoder.AbstractJsonEncoder[java.util.Currency] {
    def unsafeEncode(a: java.util.Currency, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(a.toString)
      out.write('"')
    }

    override def toJsonAST(a: java.util.Currency): Either[String, Json] =
      new Right(new Json.Str(a.toString))
  }
}

private[json] trait EncoderLowPriority4 extends EncoderLowPriorityVersionSpecific {

  /**
   * Returns the encoder of the implicit `JsonCodec[A]`, so a type that defines only a codec also has an encoder.
   */
  implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonEncoder[A] = codec.encoder
}
