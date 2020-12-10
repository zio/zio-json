package zio.json

import scala.annotation._
import scala.collection.{ immutable, mutable }
import scala.util.control.NoStackTrace

import zio.blocking._
import zio.json.JsonDecoder.JsonError
import zio.json.internal._
import zio.stream.{ Take, ZStream, ZTransducer }
import zio.{ Chunk, IO, Queue, Ref, ZIO, ZManaged }

/**
 * A `JsonDecoder[A]` instance has the ability to decode JSON to values of type `A`, potentially
 * failing with an error if the JSON content does not encode a value of the given type.
 */
trait JsonDecoder[A] { self =>

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
      case _: internal.UnexpectedEnd     => Left("Unexpected end of input")
      case _: StackOverflowError         => Left("Unexpected structure")
    }

  /**
   * Attempts to decode a stream of characters into a single value of type `A`, but may fail with
   * a human-readable exception if the stream does not encode a value of this type.
   *
   * Note: This method may not consume the full string.
   */
  final def decodeJsonStream[R <: Blocking](stream: ZStream[R, Throwable, Char]): ZIO[R, Throwable, A] =
    stream.toReader.use { reader =>
      effectBlocking {
        try unsafeDecode(Nil, new zio.json.internal.WithRetractReader(reader))
        catch {
          case JsonDecoder.UnsafeJson(trace) => throw new Exception(JsonError.render(trace))
          case _: internal.UnexpectedEnd     => throw new Exception("unexpected end of input")
        }
      }
    }

  final def decodeJsonTransducer(
    delimiter: JsonStreamDelimiter = JsonStreamDelimiter.Array
  ): ZTransducer[Blocking, Throwable, Char, A] =
    ZTransducer {
      for {
        // format: off
        runtime    <- ZIO.runtime[Any].toManaged_
        inQueue    <- Queue.unbounded[Take[Nothing, Char]].toManaged_
        outQueue   <- Queue.unbounded[Take[Throwable, A]].toManaged_
        ended      <- Ref.makeManaged(false)
        reader     <- ZManaged.fromAutoCloseable {
                        ZIO.effectTotal {
                          def readPull: Iterator[Chunk[Char]] =
                            runtime.unsafeRun(inQueue.take)
                              .fold(
                                end   = Iterator.empty,
                                error = _ => Iterator.empty, // impossible
                                value = v => Iterator.single(v) ++ readPull
                              )

                          new zio.stream.internal.ZReader(Iterator.empty ++ readPull)
                        }
                      }
        jsonReader <- ZManaged.fromAutoCloseable(ZIO.effectTotal(new WithRetractReader(reader)))
        process    <- effectBlockingInterrupt {
                        // Exceptions fall through and are pushed into the queue
                        @tailrec def loop(atBeginning: Boolean): Unit = {
                          val nextElem = try {
                            if (atBeginning && delimiter == JsonStreamDelimiter.Array)  {
                              Lexer.char(Nil, jsonReader, '[')
                            } else {
                              delimiter match {
                                case JsonStreamDelimiter.Newline =>
                                  jsonReader.readChar() match {
                                    case '\r' =>
                                      jsonReader.readChar() match {
                                        case '\n' => ()
                                        case _    => jsonReader.retract()
                                      }
                                    case '\n' => ()
                                    case _    => jsonReader.retract()
                                  }

                               case JsonStreamDelimiter.Array =>
                                  jsonReader.nextNonWhitespace() match {
                                    case ',' | ']' => ()
                                    case _         => jsonReader.retract()
                                  }
                              }
                            }

                            unsafeDecode(Nil, jsonReader)
                          } catch {
                            case t @ JsonDecoder.UnsafeJson(trace) =>
                              throw new Exception(JsonError.render(trace))
                          }

                          runtime.unsafeRun(outQueue.offer(Take.single(nextElem)))

                          loop(false)
                        }

                        loop(true)
                      }
                      .catchAll {
                        case t: internal.UnexpectedEnd =>
                          // swallow if stream ended
                          ZIO.unlessM(ended.get) {
                            outQueue.offer(Take.fail(t))
                          }

                        case t: Throwable =>
                          outQueue.offer(Take.fail(t))
                      }
                      .interruptible
                      .forkManaged
        push = { is: Option[Chunk[Char]] =>
          val pollElements: IO[Throwable, Chunk[A]] =
            outQueue
              .takeUpTo(ZStream.DefaultChunkSize)
              .flatMap { takes =>
                ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                  take.fold(ZIO.succeedNow(acc), e => ZIO.fail(e.squash), c => ZIO.succeedNow(acc ++ c))
                }
              }

          val pullRest =
            outQueue
              .takeAll
              .flatMap { takes =>
                ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                  take.fold(ZIO.succeedNow(acc), e => ZIO.fail(e.squash), c => ZIO.succeedNow(acc ++ c))
                }
              }

          is match {
            case Some(c) =>
              inQueue.offer(Take.chunk(c)) *> pollElements

            case None =>
              ended.set(true) *> inQueue.offer(Take.end) *> process.join *> pullRest
          }
        }
      } yield push
      // format: on
    }


  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: JsonDecoder[B] = self.asInstanceOf[JsonDecoder[B]]

  /**
   * Returns this decoder but narrowed to the its given sub-type
   */
  final def narrow[B <: A]: JsonEncoder[B] = self.asInstanceOf[JsonEncoder[B]]

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics:
   * such that if this codec fails, the specified codec will be tried instead.
   * This method may be unsafe from a security perspective: it can use more memory than hand coded
   * alternative and so lead to DOS.
   *
   * For example, in the case of an alternative between `Int` and `Boolean`, an hand coded
   * alternative would look like:
   *
   * ```
   * val decoder: JsonDecoder[AnyVal] = new JsonDecoder[AnyVal] {
   *   def unsafeDecode(trace: List[JsonError], in: RetractReader): AnyVal =
   *     (in.nextNonWhitespace(): @switch) match {
   *       case 't' | 'f' =>
   *         in.retract()
   *         JsonDecoder[Boolean].unsafeDecode(JsonError.SumType("Boolean") :: trace, in).asInstanceOf[AnyVal]
   *       case c =>
   *         in.retract()
   *         JsonDecoder[Int].unsafeDecode(JsonError.SumType("Int") :: trace, in).asInstanceOf[AnyVal]
   *   }
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

          case _: internal.UnexpectedEnd =>
            in2.rewind()
            that.unsafeDecode(trace, in2)
        }
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
  final def map[B](f: A => B): JsonDecoder[B] =
    new JsonDecoder[B] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))
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
}

object JsonDecoder extends GeneratedTupleDecoders with DecoderLowPriority0 {
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
    final case class Message(txt: String)        extends JsonError
    final case class ArrayAccess(i: Int)         extends JsonError
    final case class ObjectAccess(field: String) extends JsonError
    final case class SumType(cons: String)       extends JsonError
  }

  implicit def string: JsonDecoder[String] = new JsonDecoder[String] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): String =
      Lexer.string(trace, in).toString
  }
  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)
  }

  implicit val char: JsonDecoder[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }
  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  implicit val byte: JsonDecoder[Byte]   = number(Lexer.byte)
  implicit val short: JsonDecoder[Short] = number(Lexer.short)
  implicit val int: JsonDecoder[Int]     = number(Lexer.int)
  implicit def long: JsonDecoder[Long]   = number(Lexer.long)
  implicit val bigInteger: JsonDecoder[java.math.BigInteger] = number(
    Lexer.bigInteger
  )
  implicit val float: JsonDecoder[Float]   = number(Lexer.float)
  implicit val double: JsonDecoder[Double] = number(Lexer.double)
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] = number(
    Lexer.bigDecimal
  )

  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (List[JsonError], RetractReader) => A
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
    }

  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new JsonDecoder[Option[A]] {
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
          do {
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
          } while (Lexer.nextField(trace, in))

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
    if (Lexer.firstArrayElement(in)) do {
      val trace_ = JsonError.ArrayAccess(i) :: trace
      builder += A.unsafeDecode(trace_, in)
      i += 1
    } while (Lexer.nextArrayElement(trace, in))
    builder.result()
  }

  private[json] def keyValueBuilder[K, V, T[X, Y] <: Iterable[(X, Y)]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[(K, V), T[K, V]]
  )(implicit K: JsonFieldDecoder[K], V: JsonDecoder[V]): T[K, V] = {
    Lexer.char(trace, in, '{')
    if (Lexer.firstField(trace, in))
      do {
        val field  = Lexer.string(trace, in).toString
        val trace_ = JsonError.ObjectAccess(field) :: trace
        Lexer.char(trace_, in, ':')
        val value = V.unsafeDecode(trace_, in)
        builder += ((K.unsafeDecodeField(trace_, field), value))
      } while (Lexer.nextField(trace, in))
    builder.result()
  }

}

private[json] trait DecoderLowPriority0 extends DecoderLowPriority1 { this: JsonDecoder.type =>
  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] = new JsonDecoder[Chunk[A]] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[A] =
      builder(trace, in, zio.ChunkBuilder.make[A]())
  }

  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    list[A].map(lst => immutable.HashSet(lst: _*))

  implicit def hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    keyValueChunk[K, V].map(lst => immutable.HashMap(lst: _*))
}

private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 { this: JsonDecoder.type =>
  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] = new JsonDecoder[Seq[A]] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Seq[A] =
      builder(trace, in, immutable.Seq.newBuilder[A])
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

private[json] trait DecoderLowPriority3 { this: JsonDecoder.type =>
  import java.time._
  import java.time.DateTimeException
  import java.time.format.{ DateTimeFormatter, DateTimeParseException }
  import java.time.zone.ZoneRulesException

  implicit val dayOfWeek: JsonDecoder[DayOfWeek] =
    string.mapOrFail(s => parseJavaTime(DayOfWeek.valueOf, s.toUpperCase))
  implicit val duration: JsonDecoder[Duration] = long.map(Duration.ofMillis)
  implicit val instant: JsonDecoder[Instant]   = string.mapOrFail(parseJavaTime(Instant.parse, _))
  implicit val localDate: JsonDecoder[LocalDate] =
    string.mapOrFail(parseJavaTime(LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE), _))
  implicit val localDateTime: JsonDecoder[LocalDateTime] =
    string.mapOrFail(parseJavaTime(LocalDateTime.parse(_, DateTimeFormatter.ISO_LOCAL_DATE_TIME), _))
  implicit val localTime: JsonDecoder[LocalTime] =
    string.mapOrFail(parseJavaTime(LocalTime.parse(_, DateTimeFormatter.ISO_LOCAL_TIME), _))
  implicit val month: JsonDecoder[Month]       = string.mapOrFail(s => parseJavaTime(Month.valueOf, s.toUpperCase))
  implicit val monthDay: JsonDecoder[MonthDay] = string.mapOrFail(parseJavaTime(MonthDay.parse, _))
  implicit val offsetDateTime: JsonDecoder[OffsetDateTime] =
    string.mapOrFail(parseJavaTime(OffsetDateTime.parse(_, DateTimeFormatter.ISO_OFFSET_DATE_TIME), _))
  implicit val offsetTime: JsonDecoder[OffsetTime] =
    string.mapOrFail(parseJavaTime(OffsetTime.parse(_, DateTimeFormatter.ISO_OFFSET_TIME), _))
  implicit val period: JsonDecoder[Period] = string.mapOrFail(parseJavaTime(Period.parse, _))
  implicit val year: JsonDecoder[Year]     = string.mapOrFail(parseJavaTime(Year.parse, _))
  implicit val yearMonth: JsonDecoder[YearMonth] =
    string.mapOrFail(parseJavaTime(YearMonth.parse, _))
  implicit val zonedDateTime: JsonDecoder[ZonedDateTime] =
    string.mapOrFail(parseJavaTime(ZonedDateTime.parse(_, DateTimeFormatter.ISO_ZONED_DATE_TIME), _))
  implicit val zoneId: JsonDecoder[ZoneId] = string.mapOrFail(parseJavaTime(ZoneId.of, _))
  implicit val zoneOffset: JsonDecoder[ZoneOffset] =
    string.mapOrFail(parseJavaTime(ZoneOffset.of, _))

  // Commonized handling for decoding from string to java.time Class
  private[json] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try {
      Right(f(s))
    } catch {
      case zre: ZoneRulesException      => Left(s"${s} is not a valid ISO-8601 format, ${zre.getMessage}")
      case dtpe: DateTimeParseException => Left(s"${s} is not a valid ISO-8601 format, ${dtpe.getMessage}")
      case dte: DateTimeException       => Left(s"${s} is not a valid ISO-8601 format, ${dte.getMessage}")
      case ex: Exception                => Left(ex.getMessage)
    }
}

/** When decoding a JSON Object, we only allow the keys that implement this interface. */
trait JsonFieldDecoder[+A] { self =>
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
  implicit val string: JsonFieldDecoder[String] = new JsonFieldDecoder[String] {
    def unsafeDecodeField(trace: List[JsonError], in: String): String = in
  }
}
