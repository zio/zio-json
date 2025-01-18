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

import zio.json.internal.Lexer
import zio.json.uuid.UUIDParser

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
          case Left(err) => Lexer.error(err, trace)
          case Right(b)  => b
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

  implicit val uuid: JsonFieldDecoder[java.util.UUID] = mapStringOrFail { str =>
    try {
      Right(UUIDParser.unsafeParse(str))
    } catch {
      case iae: IllegalArgumentException => Left(s"Invalid UUID: ${iae.getMessage}")
    }
  }

  // use this instead of `string.mapOrFail` in supertypes (to prevent class initialization error at runtime)
  private[json] def mapStringOrFail[A](f: String => Either[String, A]): JsonFieldDecoder[A] =
    new JsonFieldDecoder[A] {
      def unsafeDecodeField(trace: List[JsonError], in: String): A =
        f(string.unsafeDecodeField(trace, in)) match {
          case Left(err)    => Lexer.error(err, trace)
          case Right(value) => value
        }
    }
}
