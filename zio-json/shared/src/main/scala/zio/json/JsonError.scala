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
 * A `JsonError` value describes the ways in which decoding could fail. This structure is used to facilitate
 * human-readable error messages during decoding failures.
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
