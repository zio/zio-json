package zio.json

import zio.Chunk
import zio.json.ast.Json
import zio.json.internal._
import zio.json.javatime.parsers
import zio.json.uuid.UUIDParser

import java.util.UUID
import scala.annotation._
import scala.collection.{ immutable, mutable }
import scala.util.control.NoStackTrace

/**
 * A `JsonError` value describes the ways in which decoding could fail. This structure is used
 * to facilitate human-readable error messages during decoding failures.
 */
sealed abstract class JsonError

object JsonError {

  def render(trace: List[JsonError]): String =
    trace.reverse.map {
      case Message(txt)        => s"($txt)"
      case ArrayAccess(i)      => s"[$i]"
      case ObjectAccess(field) => s".$field"
      case SumType(cons)       => s"{$cons}"
    }.mkString

  final case class Message(txt: String) extends JsonError

  final case class ArrayAccess(i: Int) extends JsonError

  final case class ObjectAccess(field: String) extends JsonError

  final case class SumType(cons: String) extends JsonError

}

/**
 * A `JsonDecoder[A]` instance has the ability to decode JSON to values of type `A`, potentially
 * failing with an error if the JSON content does not encode a value of the given type.
 */
trait JsonDecoder[A] extends JsonDecoderPlatformSpecific[A] {
  self =>

  /**
   * An alias for [[JsonDecoder#orElse]].
   */
  final def <>[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] = self.orElse(that)

  /**
   * An alias for [[JsonDecoder#orElseEither]].
   */
  final def <+>[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] = self.orElseEither(that)

  /**
   * An alias for [[JsonDecoder#zip]].
   */
  final def <*>[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = self.zip(that)

  /**
   * An alias for [[JsonDecoder#zipRight]].
   */
  final def *>[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipRight(that)

  /**
   * An alias for [[JsonDecoder#zipLeft]].
   */
  final def <*[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipLeft(that)

  /**
   * Attempts to decode a value of type `A` from the specified `CharSequence`, but may fail with
   * a human-readable error message if the provided text does not encode a value of this type.
   *
   * Note: This method may not entirely consume the specified character sequence.
   */
  final def decodeJson(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case JsonDecoder.UnsafeJson(trace) => Left(JsonError.render(trace))
      case _: UnexpectedEnd              => Left("Unexpected end of input")
      case _: StackOverflowError         => Left("Unexpected structure")
    }

  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: JsonDecoder[B] = self.asInstanceOf[JsonDecoder[B]]

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics:
   * such that if this codec fails, the specified codec will be tried instead.
   * This method may be unsafe from a security perspective: it can use more memory than hand coded
   * alternative and so lead to DOS.
   *
   * For example, in the case of an alternative between `Int` and `Boolean`, a hand coded
   * alternative would look like:
   *
   * ```
   * val decoder: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
   * case 't' | 'f' => JsonDecoder[Boolean].widen
   * case c         => JsonDecoder[Int].widen
   * }
   * ```
   */
  final def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] =
    new JsonDecoder[A1] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): A1 = {
        val in2 = new zio.json.internal.WithRecordingReader(in, 64)

        try self.unsafeDecode(trace, in2)
        catch {
          case JsonDecoder.UnsafeJson(_) =>
            in2.rewind()
            that.unsafeDecode(trace, in2)

          case _: UnexpectedEnd =>
            in2.rewind()
            that.unsafeDecode(trace, in2)
        }
      }

      override final def fromJsonAST(json: Json): Either[String, A1] =
        self.fromJsonAST(json) match {
          case Left(_)           => that.fromJsonAST(json)
          case result @ Right(_) => result
        }
    }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics:
   * such that if this codec fails, the specified codec will be tried instead.
   */
  final def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    self.map(Left(_)).orElse(that.map(Right(_)))

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function.
   */
  def map[B](f: A => B): JsonDecoder[B] =
    new JsonDecoder[B] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, B] =
        self.fromJsonAST(json).map(f)
    }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may
   * itself decide to fail with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    new JsonDecoder[B] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }

      override final def fromJsonAST(json: Json): Either[String, B] =
        self.fromJsonAST(json).flatMap(f)
    }

  @nowarn("msg=is never used")
  def xmap[B](f: A => B, g: B => A): JsonDecoder[B] = map(f)

  /**
   * Returns a new codec that combines this codec and the specified codec into a single codec that
   * decodes a tuple of the values decoded by the respective codecs.
   */
  final def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = JsonDecoder.tuple2(this, that)

  /**
   * Zips two codecs, but discards the output on the right hand side.
   */
  final def zipLeft[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipWith(that)((a, _) => a)

  /**
   * Zips two codecs, but discards the output on the left hand side.
   */
  final def zipRight[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipWith(that)((_, b) => b)

  /**
   * Zips two codecs into one, transforming the outputs of both codecs by the specified function.
   */
  final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] =
    self.zip(that).map(f.tupled)

  def unsafeDecodeMissing(trace: List[JsonError]): A =
    throw JsonDecoder.UnsafeJson(JsonError.Message("missing") :: trace)

  /**
   * Low-level, unsafe method to decode a value or throw an exception. This method should not be
   * called in application code, although it can be implemented for user-defined data structures.
   */
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A

  /**
   * Decode a value from an already parsed Json AST.
   *
   * The default implementation encodes the Json to a byte stream and uses decode to parse that.
   * Override to provide a more performant implementation.
   */
  def fromJsonAST(json: Json): Either[String, A] =
    decodeJson(Json.encoder.encodeJson(json, None))
}

object JsonDecoder extends GeneratedTupleDecoders with DecoderLowPriority0 {
  type JsonError = zio.json.JsonError
  val JsonError = zio.json.JsonError

  def apply[A](implicit a: JsonDecoder[A]): JsonDecoder[A] = a

  /**
   * Design note: we could require the position in the stream here to improve
   * debugging messages. But the cost would be that the RetractReader would need
   * to keep track and any wrappers would need to preserve the position. It may
   * still be desirable to do this but at the moment it is not necessary.
   */
  final case class UnsafeJson(trace: List[JsonError])
      extends Exception("If you see this, a developer made a mistake using JsonDecoder")
      with NoStackTrace

  def peekChar[A](partialFunction: PartialFunction[Char, JsonDecoder[A]]): JsonDecoder[A] = new JsonDecoder[A] {

    override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
      val c = in.nextNonWhitespace()
      if (partialFunction.isDefinedAt(c)) {
        in.retract()
        partialFunction(c).unsafeDecode(trace, in)
      } else {
        throw UnsafeJson(JsonError.Message(s"missing case in `peekChar` for '${c}''") :: trace)
      }
    }
  }

  implicit val string: JsonDecoder[String] = new JsonDecoder[String] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): String =
      Lexer.string(trace, in).toString

    override final def fromJsonAST(json: Json): Either[String, String] =
      json match {
        case Json.Str(value) => Right(value)
        case _               => Left("Not a string value")
      }
  }

  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)

    override final def fromJsonAST(json: Json): Either[String, Boolean] =
      json match {
        case Json.Bool(value) => Right(value)
        case _                => Left("Not a bool value")
      }
  }

  implicit val char: JsonDecoder[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }
  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  implicit val byte: JsonDecoder[Byte]                       = number(Lexer.byte, _.byteValueExact())
  implicit val short: JsonDecoder[Short]                     = number(Lexer.short, _.shortValueExact())
  implicit val int: JsonDecoder[Int]                         = number(Lexer.int, _.intValueExact())
  implicit val long: JsonDecoder[Long]                       = number(Lexer.long, _.longValueExact())
  implicit val bigInteger: JsonDecoder[java.math.BigInteger] = number(Lexer.bigInteger, _.toBigIntegerExact)
  implicit val scalaBigInt: JsonDecoder[BigInt]              = bigInteger.map(x => x)
  implicit val float: JsonDecoder[Float]                     = number(Lexer.float, _.floatValue())
  implicit val double: JsonDecoder[Double]                   = number(Lexer.double, _.doubleValue())
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] = number(Lexer.bigDecimal, identity)
  implicit val scalaBigDecimal: JsonDecoder[BigDecimal]      = bigDecimal.map(x => x)

  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (List[JsonError], RetractReader) => A,
    fromBigDecimal: java.math.BigDecimal => A
  ): JsonDecoder[A] =
    new JsonDecoder[A] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        (in.nextNonWhitespace(): @switch) match {
          case '"' =>
            val i = f(trace, in)
            Lexer.charOnly(trace, in, '"')
            i
          case _ =>
            in.retract()
            f(trace, in)
        }

      override final def fromJsonAST(json: Json): Either[String, A] =
        json match {
          case Json.Num(value) =>
            try Right(fromBigDecimal(value))
            catch {
              case exception: ArithmeticException => Left(exception.getMessage)
            }
          case Json.Str(value) =>
            val reader = new FastStringReader(value)
            val result =
              try Right(f(List.empty, reader))
              catch {
                case JsonDecoder.UnsafeJson(trace) => Left(JsonError.render(trace))
                case _: UnexpectedEnd              => Left("Unexpected end of input")
                case _: StackOverflowError         => Left("Unexpected structure")
              } finally reader.close()
            result
          case _ => Left("Not a number or a string")
        }
    }

  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new JsonDecoder[Option[A]] { self =>
      private[this] val ull: Array[Char] = "ull".toCharArray

      override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] =
        Option.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
        (in.nextNonWhitespace(): @switch) match {
          case 'n' =>
            Lexer.readChars(trace, in, ull, "null")
            None
          case _ =>
            in.retract()
            Some(A.unsafeDecode(trace, in))
        }

      // overridden here to pass `None` to the new Decoder instead of throwing
      // when called from a derived decoder
      override def map[B](f: Option[A] => B): JsonDecoder[B] =
        new JsonDecoder[B] {
          override def unsafeDecodeMissing(trace: List[JsonError]): B =
            f(None)

          def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
            f(self.unsafeDecode(trace, in))

          override final def fromJsonAST(json: Json): Either[String, B] =
            self.fromJsonAST(json).map(f)
        }

      override final def fromJsonAST(json: Json): Either[String, Option[A]] =
        json match {
          case Json.Null => Right(None)
          case _         => A.fromJsonAST(json).map(Some.apply)
        }
    }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](implicit
    A: JsonDecoder[A],
    B: JsonDecoder[B]
  ): JsonDecoder[Either[A, B]] =
    new JsonDecoder[Either[A, B]] {

      val names: Array[String] =
        Array("a", "Left", "left", "b", "Right", "right")
      val matrix: StringMatrix    = new StringMatrix(names)
      val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

      def unsafeDecode(
        trace: List[JsonError],
        in: RetractReader
      ): Either[A, B] = {
        Lexer.char(trace, in, '{')

        val values: Array[Any] = Array.ofDim(2)

        if (Lexer.firstField(trace, in))
          while ({
            {
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                if (field < 3) {
                  if (values(0) != null)
                    throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                  values(0) = A.unsafeDecode(trace_, in)
                } else {
                  if (values(1) != null)
                    throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                  values(1) = B.unsafeDecode(trace_, in)
                }
              }
            }; Lexer.nextField(trace, in)
          }) ()

        if (values(0) == null && values(1) == null)
          throw UnsafeJson(JsonError.Message("missing fields") :: trace)
        if (values(0) != null && values(1) != null)
          throw UnsafeJson(
            JsonError.Message("ambiguous either, both present") :: trace
          )
        if (values(0) != null)
          Left(values(0).asInstanceOf[A])
        else Right(values(1).asInstanceOf[B])
      }
    }

  private[json] def builder[A, T[_]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[A, T[A]]
  )(implicit A: JsonDecoder[A]): T[A] = {
    Lexer.char(trace, in, '[')
    var i: Int = 0
    if (Lexer.firstArrayElement(in)) while ({
      {
        val trace_ = JsonError.ArrayAccess(i) :: trace
        builder += A.unsafeDecode(trace_, in)
        i += 1
      }; Lexer.nextArrayElement(trace, in)
    }) ()
    builder.result()
  }

  private[json] def keyValueBuilder[K, V, T[X, Y] <: Iterable[(X, Y)]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[(K, V), T[K, V]]
  )(implicit K: JsonFieldDecoder[K], V: JsonDecoder[V]): T[K, V] = {
    Lexer.char(trace, in, '{')
    if (Lexer.firstField(trace, in))
      while ({
        {
          val field  = Lexer.string(trace, in).toString
          val trace_ = JsonError.ObjectAccess(field) :: trace
          Lexer.char(trace_, in, ':')
          val value = V.unsafeDecode(trace_, in)
          builder += ((K.unsafeDecodeField(trace_, field), value))
        }; Lexer.nextField(trace, in)
      }) ()
    builder.result()
  }

  // use this instead of `string.mapOrFail` in supertypes (to prevent class initialization error at runtime)
  private[json] def mapStringOrFail[A](f: String => Either[String, A]): JsonDecoder[A] =
    new JsonDecoder[A] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        f(string.unsafeDecode(trace, in)) match {
          case Left(err)    => throw UnsafeJson(JsonError.Message(err) :: trace)
          case Right(value) => value
        }

      override def fromJsonAST(json: Json): Either[String, A] =
        string.fromJsonAST(json).flatMap(f)
    }
}

private[json] trait DecoderLowPriority0 extends DecoderLowPriority1 {
  this: JsonDecoder.type =>

  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] = new JsonDecoder[Chunk[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[A] =
      builder(trace, in, zio.ChunkBuilder.make[A]())

    override final def fromJsonAST(json: Json): Either[String, Chunk[A]] =
      json match {
        case Json.Arr(elements) =>
          elements.foldLeft[Either[String, Chunk[A]]](Right(Chunk.empty)) { (s, item) =>
            s.flatMap(chunk =>
              implicitly[JsonDecoder[A]].fromJsonAST(item).map { a =>
                chunk :+ a
              }
            )
          }
        case _ => Left("Not an array")
      }
  }

  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    list[A].map(lst => immutable.HashSet(lst: _*))

  implicit def hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    keyValueChunk[K, V].map(lst => immutable.HashMap(lst: _*))
}

private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 {
  this: JsonDecoder.type =>

  implicit def array[A: JsonDecoder: reflect.ClassTag]: JsonDecoder[Array[A]] = new JsonDecoder[Array[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Array[A] =
      builder(trace, in, Array.newBuilder[A])
  }

  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] = new JsonDecoder[Seq[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Seq[A] =
      builder(trace, in, immutable.Seq.newBuilder[A])
  }

  implicit def indexedSeq[A: JsonDecoder]: JsonDecoder[IndexedSeq[A]] =
    new JsonDecoder[IndexedSeq[A]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader) =
        builder(trace, in, IndexedSeq.newBuilder[A])
    }

  implicit def linearSeq[A: JsonDecoder]: JsonDecoder[immutable.LinearSeq[A]] =
    new JsonDecoder[immutable.LinearSeq[A]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader) =
        builder(trace, in, immutable.LinearSeq.newBuilder[A])
    }

  implicit def listSet[A: JsonDecoder]: JsonDecoder[immutable.ListSet[A]] = new JsonDecoder[immutable.ListSet[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader) =
      builder(trace, in, immutable.ListSet.newBuilder[A])
  }

  implicit def treeSet[A: JsonDecoder: Ordering]: JsonDecoder[immutable.TreeSet[A]] =
    new JsonDecoder[immutable.TreeSet[A]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader) =
        builder(trace, in, immutable.TreeSet.newBuilder[A])
    }

  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] = new JsonDecoder[List[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): List[A] =
      builder(trace, in, new mutable.ListBuffer[A])
  }

  implicit def vector[A: JsonDecoder]: JsonDecoder[Vector[A]] = new JsonDecoder[Vector[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Vector[A] =
      builder(trace, in, immutable.Vector.newBuilder[A])
  }

  implicit def set[A: JsonDecoder]: JsonDecoder[Set[A]] = new JsonDecoder[Set[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Set[A] =
      builder(trace, in, immutable.HashSet.newBuilder[A])
  }

  implicit def map[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    new JsonDecoder[Map[K, V]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Map[K, V] =
        keyValueBuilder(trace, in, Map.newBuilder[K, V])
    }

  implicit def mutableMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[mutable.Map[K, V]] =
    new JsonDecoder[mutable.Map[K, V]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): mutable.Map[K, V] =
        keyValueBuilder(trace, in, mutable.Map.newBuilder[K, V])
    }

  implicit def sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[immutable.SortedSet[A]] =
    new JsonDecoder[immutable.SortedSet[A]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.SortedSet[A] =
        builder(trace, in, immutable.SortedSet.newBuilder[A])
    }

  implicit def sortedMap[K: JsonFieldDecoder: Ordering, V: JsonDecoder]: JsonDecoder[collection.SortedMap[K, V]] =
    new JsonDecoder[collection.SortedMap[K, V]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): collection.SortedMap[K, V] =
        keyValueBuilder(trace, in, collection.SortedMap.newBuilder[K, V])
    }
}

// We have a hierarchy of implicits for two reasons:
//
// 1. the compiler searches each scope and returns early if it finds a match.
//    This means that it is faster to put more complex derivation rules (that
//    are unlikely to be commonly used) into a lower priority scope, allowing
//    simple things like primitives to match fast.
//
// 2. sometimes we want to have overlapping instances with a more specific /
//    optimised instances, and a fallback for the more general case that would
//    otherwise conflict in a lower priority scope. A good example of this is to
//    have specialised decoders for collection types, falling back to BuildFrom.
private[json] trait DecoderLowPriority2 extends DecoderLowPriority3 {
  this: JsonDecoder.type =>

  implicit def iterable[A: JsonDecoder]: JsonDecoder[Iterable[A]] = new JsonDecoder[Iterable[A]] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Iterable[A] =
      builder(trace, in, immutable.Iterable.newBuilder[A])
  }

  // not implicit because this overlaps with decoders for lists of tuples
  def keyValueChunk[K, A](implicit
    K: JsonFieldDecoder[K],
    A: JsonDecoder[A]
  ): JsonDecoder[Chunk[(K, A)]] =
    new JsonDecoder[Chunk[(K, A)]] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[(K, A)] =
        keyValueBuilder[K, A, ({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda](
          trace,
          in,
          zio.ChunkBuilder.make[(K, A)]()
        )
    }

}

private[json] trait DecoderLowPriority3 {
  this: JsonDecoder.type =>

  import java.time.{ DateTimeException, _ }
  import java.time.format.DateTimeParseException
  import java.time.zone.ZoneRulesException

  implicit val dayOfWeek: JsonDecoder[DayOfWeek] = mapStringOrFail(s => parseJavaTime(DayOfWeek.valueOf, s.toUpperCase))
  implicit val duration: JsonDecoder[Duration]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseDuration, _))
  implicit val instant: JsonDecoder[Instant]     = mapStringOrFail(parseJavaTime(Instant.parse, _))
  implicit val localDate: JsonDecoder[LocalDate] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDate, _))

  implicit val localDateTime: JsonDecoder[LocalDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDateTime, _))

  implicit val localTime: JsonDecoder[LocalTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalTime, _))
  implicit val month: JsonDecoder[Month]         = mapStringOrFail(s => parseJavaTime(Month.valueOf, s.toUpperCase))
  implicit val monthDay: JsonDecoder[MonthDay]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseMonthDay, _))

  implicit val offsetDateTime: JsonDecoder[OffsetDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetDateTime, _))

  implicit val offsetTime: JsonDecoder[OffsetTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetTime, _))
  implicit val period: JsonDecoder[Period]         = mapStringOrFail(parseJavaTime(Period.parse, _))
  implicit val year: JsonDecoder[Year]             = mapStringOrFail(parseJavaTime(parsers.unsafeParseYear, _))
  implicit val yearMonth: JsonDecoder[YearMonth]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseYearMonth, _))

  implicit val zonedDateTime: JsonDecoder[ZonedDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseZonedDateTime, _))

  implicit val zoneId: JsonDecoder[ZoneId] = mapStringOrFail(parseJavaTime(ZoneId.of, _))

  implicit val zoneOffset: JsonDecoder[ZoneOffset] =
    mapStringOrFail(parseJavaTime(ZoneOffset.of, _))

  // Commonized handling for decoding from string to java.time Class
  private[json] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try {
      Right(f(s))
    } catch {
      case zre: ZoneRulesException      => Left(s"$s is not a valid ISO-8601 format, ${zre.getMessage}")
      case dtpe: DateTimeParseException => Left(s"$s is not a valid ISO-8601 format, ${dtpe.getMessage}")
      case dte: DateTimeException       => Left(s"$s is not a valid ISO-8601 format, ${dte.getMessage}")
      case ex: Exception                => Left(ex.getMessage)
    }

  implicit val uuid: JsonDecoder[UUID] =
    mapStringOrFail { str =>
      try {
        Right(UUIDParser.unsafeParse(str))
      } catch {
        case iae: IllegalArgumentException =>
          Left(s"Invalid UUID: ${iae.getMessage}")
      }
    }
}

/** When decoding a JSON Object, we only allow the keys that implement this interface. */
trait JsonFieldDecoder[+A] {
  self =>

  final def map[B](f: A => B): JsonFieldDecoder[B] =
    new JsonFieldDecoder[B] {

      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in))
    }

  final def mapOrFail[B](f: A => Either[String, B]): JsonFieldDecoder[B] =
    new JsonFieldDecoder[B] {

      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }
    }

  def unsafeDecodeField(trace: List[JsonError], in: String): A
}

object JsonFieldDecoder {
  def apply[A](implicit a: JsonFieldDecoder[A]): JsonFieldDecoder[A] = a

  implicit val string: JsonFieldDecoder[String] = new JsonFieldDecoder[String] {
    def unsafeDecodeField(trace: List[JsonError], in: String): String = in
  }

  implicit val int: JsonFieldDecoder[Int] =
    JsonFieldDecoder[String].mapOrFail { str =>
      try {
        Right(str.toInt)
      } catch {
        case n: NumberFormatException => Left(s"Invalid Int: '$str': $n")
      }
    }

  implicit val long: JsonFieldDecoder[Long] =
    JsonFieldDecoder[String].mapOrFail { str =>
      try {
        Right(str.toLong)
      } catch {
        case n: NumberFormatException => Left(s"Invalid Long: '$str': $n")
      }
    }
}
