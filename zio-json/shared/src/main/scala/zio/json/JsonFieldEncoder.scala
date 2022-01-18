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

/** When encoding a JSON Object, we only allow keys that implement this interface. */
trait JsonFieldEncoder[-A] {
  self =>

  final def contramap[B](f: B => A): JsonFieldEncoder[B] = new JsonFieldEncoder[B] {
    override def unsafeEncodeField(in: B): String = self.unsafeEncodeField(f(in))
  }

  def unsafeEncodeField(in: A): String
}

object JsonFieldEncoder {
  def apply[A](implicit a: JsonFieldEncoder[A]): JsonFieldEncoder[A] = a

  implicit val string: JsonFieldEncoder[String] = new JsonFieldEncoder[String] {
    def unsafeEncodeField(in: String): String = in
  }

  implicit val int: JsonFieldEncoder[Int] =
    JsonFieldEncoder[String].contramap(_.toString)

  implicit val long: JsonFieldEncoder[Long] =
    JsonFieldEncoder[String].contramap(_.toString)
}
