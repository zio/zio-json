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

/**
 * A type class that turns a map key of type `A` into the string used as a JSON object key. The encoders for `Map` and
 * the other map types need one for their key type. The companion has instances for `String`, `Int`, `Long` and
 * `java.util.UUID`, and [[JsonFieldEncoder#contramap]] builds one for a type of your own. The map encoder puts the
 * string in quotes and escapes it.
 *
 * {{{
 * final case class Sku(code: String)
 *
 * implicit val skuKey: JsonFieldEncoder[Sku] = JsonFieldEncoder[String].contramap(_.code)
 *
 * val stock: String = Map(Sku("AB-12") -> 3, Sku("CD-34") -> 0).toJson
 * // {"AB-12":3,"CD-34":0}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/encoding/ Encoding]]
 */
trait JsonFieldEncoder[-A] {
  self =>

  /**
   * Returns a key encoder for `B` that converts each key to `A` with `f` and then encodes it with this key encoder.
   */
  final def contramap[B](f: B => A): JsonFieldEncoder[B] = new JsonFieldEncoder[B] {
    override def unsafeEncodeField(in: B): String = self.unsafeEncodeField(f(in))
  }

  /**
   * Returns the JSON object key for `in`, without quotes or escaping: the map encoder adds both.
   */
  def unsafeEncodeField(in: A): String
}

/**
 * Built-in [[JsonFieldEncoder]] instances, found without an import.
 */
object JsonFieldEncoder {

  /**
   * Returns the implicit `JsonFieldEncoder[A]` in scope.
   */
  def apply[A](implicit a: JsonFieldEncoder[A]): JsonFieldEncoder[A] = a

  /**
   * Uses a `String` key as it is.
   */
  implicit val string: JsonFieldEncoder[String] = new JsonFieldEncoder[String] {
    def unsafeEncodeField(in: String): String = in
  }

  /**
   * Writes an `Int` key in decimal, such as `-7`.
   */
  implicit val int: JsonFieldEncoder[Int] = new JsonFieldEncoder[Int] {
    def unsafeEncodeField(in: Int): String = in.toString
  }

  /**
   * Writes a `Long` key in decimal, such as `9000000000`.
   */
  implicit val long: JsonFieldEncoder[Long] = new JsonFieldEncoder[Long] {
    def unsafeEncodeField(in: Long): String = in.toString
  }

  /**
   * Writes a `java.util.UUID` key in its lower-case canonical form, such as `123e4567-e89b-12d3-a456-426614174000`.
   */
  implicit val uuid: JsonFieldEncoder[java.util.UUID] = new JsonFieldEncoder[java.util.UUID] {
    def unsafeEncodeField(in: java.util.UUID): String = in.toString
  }
}
