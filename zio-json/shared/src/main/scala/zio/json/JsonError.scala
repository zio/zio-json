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
 * One step of the path to a decoding failure. A decoder that fails throws `JsonDecoder.UnsafeJson` with a list of
 * `JsonError` values, called the trace, and `JsonError.render` turns the trace into the `String` that `decodeJson` and
 * `fromJson` return in a `Left`.
 *
 * `ObjectAccess`, `ArrayAccess` and `SumType` are steps into an object field, an array element and a sum type case, and
 * `Message` is the failure itself. The trace keeps the innermost step first.
 *
 * {{{
 * final case class Line(sku: String, qty: Int)
 * final case class Order(lines: List[Line])
 *
 * implicit val lineDecoder: JsonDecoder[Line]   = DeriveJsonDecoder.gen[Line]
 * implicit val orderDecoder: JsonDecoder[Order] = DeriveJsonDecoder.gen[Order]
 *
 * val result = """{"lines":[{"sku":"A1","qty":"two"}]}""".fromJson[Order]
 * // Left(.lines[0].qty(expected an Int))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/decoding/ Decoding]]
 */
sealed abstract class JsonError

/** The four kinds of path step, and `JsonError.render`, which turns a trace into a message. */
object JsonError {

  /**
   * Renders a trace as a path followed by the failure message, outermost step first: `.field` for `ObjectAccess`, `[i]`
   * for `ArrayAccess`, `{cons}` for `SumType` and `(txt)` for `Message`.
   *
   * {{{
   * val path = JsonError.render(
   *   List(
   *     JsonError.Message("expected an Int"),
   *     JsonError.ObjectAccess("qty"),
   *     JsonError.ArrayAccess(2),
   *     JsonError.ObjectAccess("lines")
   *   )
   * )
   * // .lines[2].qty(expected an Int)
   * }}}
   *
   * @param trace
   *   the steps with the innermost first, as decoders build them
   * @return
   *   the rendered path, or an empty string for an empty trace
   */
  def render(trace: List[JsonError]): String =
    trace
      .foldRight(new java.lang.StringBuilder) { (err, sb) =>
        err match {
          case o: ObjectAccess => sb.append('.').append(o.field)
          case a: ArrayAccess  => sb.append('[').append(a.i).append(']')
          case s: SumType      => sb.append('{').append(s.cons).append('}')
          case m: Message      => sb.append('(').append(m.txt).append(')')
        }
      }
      .toString

  /** The failure message, rendered as `(txt)`, for example `(missing)` or `(expected an Int)`. */
  final case class Message(txt: String) extends JsonError

  /** A step into the array element at index `i`, rendered as `[i]`. */
  final case class ArrayAccess(i: Int) extends JsonError

  /** A step into the object field named `field`, rendered as `.field`. Map decoders also use it for the key. */
  final case class ObjectAccess(field: String) extends JsonError

  /**
   * A step into the sum type case named `cons`, rendered as `{cons}`. No decoder in zio-json adds this step, so it
   * appears only in traces built by hand-written decoders.
   */
  final case class SumType(cons: String) extends JsonError
}
