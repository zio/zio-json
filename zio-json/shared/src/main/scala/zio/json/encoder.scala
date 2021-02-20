package zio.json

import java.time.format.{ DateTimeFormatterBuilder, SignStyle }
import java.time.temporal.ChronoField.YEAR

import scala.annotation._
import scala.collection.immutable

import zio.Chunk
import zio.json.internal.{ FastStringWrite, Write }

trait JsonEncoder[A] extends JsonEncoderPlatformSpecific[A] { self =>

  /**
   * Returns a new encoder that is capable of encoding a tuple containing the values of this
   * encoder and the specified encoder.
   */
  final def both[B](that: => JsonEncoder[B]): JsonEncoder[(A, B)] = JsonEncoder.tuple2(self, that)

  /**
   * Returns a new encoder that is capable of encoding a user-defined value, which is create from
   * a tuple of the values of this encoder and the specified encoder, from the specified user-
   * defined function.
   */
  final def bothWith[B, C](that: => JsonEncoder[B])(f: C => (A, B)): JsonEncoder[C] = self.both(that).contramap(f)

  /**
   * Returns a new encoder, with a new input type, which can be transformed to the old input type
   * by the specified user-defined function.
   */
  final def contramap[B](f: B => A): JsonEncoder[B] = new JsonEncoder[B] {

    override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
      self.unsafeEncode(f(b), indent, out)
    override def isNothing(b: B): Boolean = self.isNothing(f(b))
  }

  /**
   * Returns a new encoder that can accepts an `Either[A, B]` to either, and uses either this
   * encoder or the specified encoder to encode the two different types of values.
   */
  final def either[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]] = JsonEncoder.either[A, B](self, that)

  /**
   * Returns a new encoder with a new input type, which can be transformed to either the input
   * type of this encoder, or the input type of the specified encoder, using the user-defined
   * transformation function.
   */
  final def eitherWith[B, C](that: => JsonEncoder[B])(f: C => Either[A, B]): JsonEncoder[C] =
    self.either(that).contramap(f)

  /**
   * Encodes the specified value into a JSON string, with the specified indentation level.
   */
  final def encodeJson(a: A, indent: Option[Int]): CharSequence = {
    val writer = new FastStringWrite(64)
    unsafeEncode(a, indent, writer)
    writer.buffer
  }

  /**
   * This default may be overriden when this value may be missing within a JSON object and still
   * be encoded.
   */
  @nowarn("msg=is never used")
  def isNothing(a: A): Boolean = false

  @nowarn("msg=is never used")
  def xmap[B](f: A => B, g: B => A): JsonEncoder[B] = contramap(g)

  def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit

  /**
   * Returns this encoder but narrowed to the its given sub-type
   */
  final def narrow[B <: A]: JsonEncoder[B] = self.asInstanceOf[JsonEncoder[B]]
}

object JsonEncoder extends GeneratedTupleEncoders with EncoderLowPriority0 {
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

  }

  private[json] def explicit[A](f: A => String): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write(f(a))
  }

  private[json] def stringify[A](f: A => String): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write(s""""${f(a)}"""")
  }

  implicit val boolean: JsonEncoder[Boolean]                 = explicit(_.toString)
  implicit val char: JsonEncoder[Char]                       = string.contramap(_.toString)
  implicit val symbol: JsonEncoder[Symbol]                   = string.contramap(_.name)
  implicit val byte: JsonEncoder[Byte]                       = explicit(_.toString)
  implicit val short: JsonEncoder[Short]                     = explicit(_.toString)
  implicit val int: JsonEncoder[Int]                         = explicit(_.toString)
  implicit val long: JsonEncoder[Long]                       = explicit(_.toString)
  implicit val bigInteger: JsonEncoder[java.math.BigInteger] = explicit(_.toString)

  implicit val double: JsonEncoder[Double] = explicit { n =>
    if (n.isNaN || n.isInfinite) s""""$n""""
    else n.toString
  }
  implicit val float: JsonEncoder[Float]                     = double.contramap(_.toDouble)
  implicit val bigDecimal: JsonEncoder[java.math.BigDecimal] = explicit(_.toString)
  implicit val scalaBigDecimal: JsonEncoder[BigDecimal]      = explicit(_.toString)

  implicit def option[A](implicit A: JsonEncoder[A]): JsonEncoder[Option[A]] = new JsonEncoder[Option[A]] {

    def unsafeEncode(oa: Option[A], indent: Option[Int], out: Write): Unit = oa match {
      case None    => out.write("null")
      case Some(a) => A.unsafeEncode(a, indent, out)
    }
    override def isNothing(a: Option[A]): Boolean = a.isEmpty
  }

  def bump(indent: Option[Int]): Option[Int] = indent match {
    case None    => None
    case Some(i) => Some(i + 1)
  }

  def pad(indent: Option[Int], out: Write): Unit =
    indent.foreach(i => out.write("\n" + (" " * 2 * i)))

  implicit def either[A, B](implicit A: JsonEncoder[A], B: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    new JsonEncoder[Either[A, B]] {

      def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: Write): Unit = {
        out.write('{')
        val indent_ = bump(indent)
        pad(indent_, out)
        eab match {
          case Left(a) =>
            out.write("\"Left\"")
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            A.unsafeEncode(a, indent_, out)
          case Right(b) =>
            out.write("\"Right\"")
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            B.unsafeEncode(b, indent_, out)
        }
        pad(indent, out)
        out.write('}')
      }
    }
}

private[json] trait EncoderLowPriority0 extends EncoderLowPriority1 { this: JsonEncoder.type =>
  implicit def chunk[A: JsonEncoder]: JsonEncoder[Chunk[A]] = seq[A].contramap(_.toSeq)

  implicit def hashSet[A: JsonEncoder]: JsonEncoder[immutable.HashSet[A]] =
    list[A].contramap(_.toList)

  implicit def hashMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.HashMap[K, V]] =
    keyValueChunk[K, V].contramap(Chunk.fromIterable(_))
}

private[json] trait EncoderLowPriority1 extends EncoderLowPriority2 { this: JsonEncoder.type =>
  implicit def seq[A: JsonEncoder]: JsonEncoder[Seq[A]]       = iterable[A, Seq]
  implicit def list[A: JsonEncoder]: JsonEncoder[List[A]]     = iterable[A, List]
  implicit def vector[A: JsonEncoder]: JsonEncoder[Vector[A]] = iterable[A, Vector]
  implicit def set[A: JsonEncoder]: JsonEncoder[Set[A]]       = iterable[A, Set]

  implicit def map[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[Map[K, V]] =
    keyValueIterable[K, V, Map]

  implicit def sortedMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[collection.SortedMap[K, V]] =
    keyValueIterable[K, V, collection.SortedMap]

  implicit def sortedSet[A: Ordering: JsonEncoder]: JsonEncoder[immutable.SortedSet[A]] =
    list[A].contramap(_.toList)
}

private[json] trait EncoderLowPriority2 extends EncoderLowPriority3 { this: JsonEncoder.type =>

  implicit def iterable[A, T[X] <: Iterable[X]](implicit A: JsonEncoder[A]): JsonEncoder[T[A]] =
    new JsonEncoder[T[A]] {

      def unsafeEncode(as: T[A], indent: Option[Int], out: Write): Unit = {
        out.write('[')
        var first = true
        as.foreach { a =>
          if (first) first = false
          else if (indent.isEmpty) out.write(',')
          else out.write(", ")
          A.unsafeEncode(a, indent, out)
        }
        out.write(']')
      }
    }

  // not implicit because this overlaps with encoders for lists of tuples
  def keyValueIterable[K, A, T[X, Y] <: Iterable[(X, Y)]](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[T[K, A]] = new JsonEncoder[T[K, A]] {

    def unsafeEncode(kvs: T[K, A], indent: Option[Int], out: Write): Unit = {
      if (kvs.isEmpty) return out.write("{}")

      out.write('{')
      val indent_ = bump(indent)
      pad(indent_, out)
      var first = true
      kvs.foreach { case (k, a) =>
        if (!A.isNothing(a)) {
          if (first)
            first = false
          else {
            out.write(',')
            if (!indent.isEmpty)
              pad(indent_, out)
          }

          string.unsafeEncode(K.unsafeEncodeField(k), indent_, out)
          if (indent.isEmpty) out.write(':')
          else out.write(" : ")
          A.unsafeEncode(a, indent_, out)
        }
      }
      pad(indent, out)
      out.write('}')
    }
  }

  // not implicit because this overlaps with encoders for lists of tuples
  def keyValueChunk[K, A](implicit
    K: JsonFieldEncoder[K],
    A: JsonEncoder[A]
  ): JsonEncoder[({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda[K, A]] =
    keyValueIterable[K, A, ({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda]
}

private[json] trait EncoderLowPriority3 { this: JsonEncoder.type =>
  import java.time._
  import java.time.format.DateTimeFormatter

  implicit val dayOfWeek: JsonEncoder[DayOfWeek] = stringify(_.toString)
  implicit val duration: JsonEncoder[Duration]   = stringify(_.toString)
  implicit val instant: JsonEncoder[Instant]     = stringify(_.toString)

  implicit val localDate: JsonEncoder[LocalDate] = stringify(
    _.format(DateTimeFormatter.ISO_LOCAL_DATE)
  )

  implicit val localDateTime: JsonEncoder[LocalDateTime] = stringify(
    _.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  )

  implicit val localTime: JsonEncoder[LocalTime] = stringify(
    _.format(DateTimeFormatter.ISO_LOCAL_TIME)
  )
  implicit val month: JsonEncoder[Month]       = stringify(_.toString)
  implicit val monthDay: JsonEncoder[MonthDay] = stringify(_.toString)

  implicit val offsetDateTime: JsonEncoder[OffsetDateTime] = stringify(
    _.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
  )

  implicit val offsetTime: JsonEncoder[OffsetTime] = stringify(
    _.format(DateTimeFormatter.ISO_OFFSET_TIME)
  )
  implicit val period: JsonEncoder[Period] = stringify(_.toString)

  private val yearFormatter =
    new DateTimeFormatterBuilder().appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD).toFormatter
  implicit val year: JsonEncoder[Year]           = stringify(_.format(yearFormatter))
  implicit val yearMonth: JsonEncoder[YearMonth] = stringify(_.toString)

  implicit val zonedDateTime: JsonEncoder[ZonedDateTime] = stringify(
    _.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
  )
  implicit val zoneId: JsonEncoder[ZoneId]         = stringify(_.toString)
  implicit val zoneOffset: JsonEncoder[ZoneOffset] = stringify(_.toString)
}

/** When encoding a JSON Object, we only allow keys that implement this interface. */
trait JsonFieldEncoder[-A] { self =>

  final def contramap[B](f: B => A): JsonFieldEncoder[B] = new JsonFieldEncoder[B] {
    override def unsafeEncodeField(in: B): String = self.unsafeEncodeField(f(in))
  }

  def unsafeEncodeField(in: A): String
}

object JsonFieldEncoder {

  implicit val string: JsonFieldEncoder[String] = new JsonFieldEncoder[String] {
    def unsafeEncodeField(in: String): String = in
  }
}
