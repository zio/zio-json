package zio.json

import scala.annotation._
import scala.collection.mutable
import scala.collection.immutable

import zio.{ Chunk, Managed, Queue, UIO, ZIO, ZManaged }
import zio.blocking._
import zio.stream._

import java.io.{ BufferedWriter, Writer }

trait Encoder[-A] { self =>
  final def both[B](that: => Encoder[B]): Encoder[(A, B)] = Encoder.tuple2(self, that)

  final def bothWith[B, C](that: => Encoder[B])(f: C => (A, B)): Encoder[C] = self.both(that).contramap(f)

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    override def unsafeEncode(b: B, indent: Option[Int], out: java.io.Writer): Unit =
      self.unsafeEncode(f(b), indent, out)
    override def isNothing(b: B): Boolean = self.isNothing(f(b))
  }

  final def either[B](that: => Encoder[B]): Encoder[Either[A, B]] = Encoder.either[A, B](self, that)

  final def eitherWith[B, C](that: => Encoder[B])(f: C => Either[A, B]): Encoder[C] = self.either(that).contramap(f)

  final def encodeJson(a: A, indent: Option[Int]): String = {
    val writer = new zio.json.internal.FastStringWriter(64)
    unsafeEncode(a, indent, writer)
    writer.toString
  }

  // TODO: Use `Take` so we can push errors into the stream
  final def encodeJsonStream(a: A, indent: Option[Int]): ZStream[Blocking, Nothing, Char] =
    ZStream.unwrapManaged {
      (for {
        runtime <- ZIO.runtime[Any].toManaged_
        queue   <- Queue.bounded[Chunk[Char]](1).toManaged_
        writer <- ZManaged.fromAutoCloseable {
                   ZIO.effectTotal {
                     new java.io.BufferedWriter(new Writer {
                       override def write(buffer: Array[Char], offset: Int, len: Int): Unit =
                         runtime.unsafeRun(queue.offer(Chunk.fromArray(buffer).drop(offset).take(len)))

                        override def close(): Unit = 
                          runtime.unsafeRun(queue.shutdown)

                        override def flush(): Unit = ()
                     }, Stream.DefaultChunkSize)
                   }
                 }
        _ <- effectBlocking {
              try {
                unsafeEncode(a, indent, writer); runtime.unsafeRun(queue.shutdown)
              } catch {
                case _: Throwable => runtime.unsafeRun(queue.shutdown)
              }
            }.toManaged_.fork
      } yield ZStream.fromChunkQueue(queue))
    }

  // override and return `true` when this value may be skipped from JSON Objects
  def isNothing(a: A): Boolean = false

  def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit
}

object Encoder extends GeneratedTupleEncoders with EncoderLowPriority0 {
  def apply[A](implicit a: Encoder[A]): Encoder[A] = a

  implicit val string: Encoder[String] = new Encoder[String] {
    override def unsafeEncode(a: String, indent: Option[Int], out: java.io.Writer): Unit = {
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

  private[this] def explicit[A](f: A => String): Encoder[A] = new Encoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = out.write(f(a))
  }
  implicit val boolean: Encoder[Boolean] = explicit(_.toString)
  implicit val char: Encoder[Char]       = string.contramap(_.toString)
  implicit val symbol: Encoder[Symbol]   = string.contramap(_.name)

  implicit val byte: Encoder[Byte]                       = explicit(_.toString)
  implicit val short: Encoder[Short]                     = explicit(_.toString)
  implicit val int: Encoder[Int]                         = explicit(_.toString)
  implicit val long: Encoder[Long]                       = explicit(_.toString)
  implicit val bigInteger: Encoder[java.math.BigInteger] = explicit(_.toString)
  implicit val double: Encoder[Double] = explicit { n =>
    if (n.isNaN || n.isInfinite) s""""$n""""
    else n.toString
  }
  implicit val float: Encoder[Float]                     = double.contramap(_.toDouble)
  implicit val bigDecimal: Encoder[java.math.BigDecimal] = explicit(_.toString)

  implicit def option[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    def unsafeEncode(oa: Option[A], indent: Option[Int], out: java.io.Writer): Unit = oa match {
      case None    => out.write("null")
      case Some(a) => A.unsafeEncode(a, indent, out)
    }
    override def isNothing(a: Option[A]): Boolean = a.isEmpty
  }

  def bump(indent: Option[Int]): Option[Int] = indent match {
    case None    => None
    case Some(i) => Some(i + 1)
  }
  def pad(indent: Option[Int], out: java.io.Writer): Unit =
    indent.foreach(i => out.write("\n" + (" " * 2 * i)))

  implicit def either[A, B](implicit A: Encoder[A], B: Encoder[B]): Encoder[Either[A, B]] = new Encoder[Either[A, B]] {
    def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: java.io.Writer): Unit = {
      out.write("{")
      val indent_ = bump(indent)
      pad(indent_, out)
      eab match {
        case Left(a) =>
          out.write("\"Left\"")
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          A.unsafeEncode(a, indent_, out)
        case Right(b) =>
          out.write("\"Right\"")
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          B.unsafeEncode(b, indent_, out)
      }
      pad(indent, out)
      out.write("}")
    }
  }
}

private[json] trait EncoderLowPriority0 extends EncoderLowPriority1 { this: Encoder.type =>
  implicit def chunk[A: Encoder]: Encoder[Chunk[A]] = seq[A]

  implicit def list[A: Encoder]: Encoder[List[A]]     = seq[A]
  implicit def vector[A: Encoder]: Encoder[Vector[A]] = seq[A]

  implicit def hashSet[A: Encoder]: Encoder[immutable.HashSet[A]] =
    list[A].contramap(_.toList)

  implicit def hashMap[K: FieldEncoder, V: Encoder]: Encoder[immutable.HashMap[K, V]] =
    keyValueChunk[K, V].contramap(Chunk.fromIterable(_))

  // TODO these could be optimised...
  implicit def sortedMap[K: FieldEncoder, V: Encoder]: Encoder[collection.SortedMap[K, V]] =
    keyValueChunk[K, V].contramap(Chunk.fromIterable(_))

  implicit def sortedSet[A: Ordering: Encoder]: Encoder[immutable.SortedSet[A]] =
    list[A].contramap(_.toList)
}

private[json] trait EncoderLowPriority1 { this: Encoder.type =>
  implicit def seq[A](implicit A: Encoder[A]): Encoder[Seq[A]] = new Encoder[Seq[A]] {
    def unsafeEncode(as: Seq[A], indent: Option[Int], out: java.io.Writer): Unit = {
      out.write("[")
      var first = true
      as.foreach { a =>
        if (first) first = false
        else if (indent.isEmpty) out.write(",")
        else out.write(", ")
        A.unsafeEncode(a, indent, out)
      }
      out.write("]")
    }
  }

  // not implicit because this overlaps with encoders for lists of tuples
  def keyValueChunk[K, A](
    implicit
    K: FieldEncoder[K],
    A: Encoder[A]
  ): Encoder[Chunk[(K, A)]] = new Encoder[Chunk[(K, A)]] {
    def unsafeEncode(kvs: Chunk[(K, A)], indent: Option[Int], out: java.io.Writer): Unit = {
      if (kvs.isEmpty) return out.write("{}")

      out.write("{")
      val indent_ = bump(indent)
      pad(indent_, out)
      var first = true
      kvs.foreach {
        case (k, a) =>
          if (!A.isNothing(a)) {
            if (first)
              first = false
            else if (indent.isEmpty)
              out.write(",")
            else {
              out.write(",")
              pad(indent_, out)
            }

            string.unsafeEncode(K.unsafeEncodeField(k), indent_, out)
            if (indent.isEmpty) out.write(":")
            else out.write(" : ")
            A.unsafeEncode(a, indent_, out)
          }
      }
      pad(indent, out)
      out.write("}")
    }
  }

  implicit def map[K: FieldEncoder, V: Encoder]: Encoder[Map[K, V]] =
    keyValueChunk[K, V].contramap(Chunk.fromIterable(_))
  implicit def set[A: Encoder]: Encoder[Set[A]] =
    list[A].contramap(_.toList)
}

/** When encoding a JSON Object, we only allow keys that implement this interface. */
trait FieldEncoder[-A] { self =>

  final def contramap[B](f: B => A): FieldEncoder[B] = new FieldEncoder[B] {
    override def unsafeEncodeField(in: B): String = self.unsafeEncodeField(f(in))
  }

  def unsafeEncodeField(in: A): String
}
object FieldEncoder {
  implicit val string: FieldEncoder[String] = new FieldEncoder[String] {
    def unsafeEncodeField(in: String): String = in
  }
}
