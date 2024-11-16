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
package zio

import zio.json.ast.Json

package object json extends JsonPackagePlatformSpecific {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit encoder: JsonEncoder[A]): String = encoder.encodeJson(a, None).toString

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit encoder: JsonEncoder[A]): String = encoder.encodeJson(a, Some(0)).toString

    def toJsonAST(implicit encoder: JsonEncoder[A]): Either[String, Json] = encoder.toJsonAST(a)
  }

  implicit final class DecoderOps(private val json: CharSequence) extends AnyVal {

    /**
     * Attempts to decode the raw JSON string as an `A`.
     *
     * On failure a human readable message is returned using a jq friendly format. For example the error
     * `.rows[0].elements[0].distance.value(missing)"` tells us the location of a missing field named "value". We can
     * use part of the error message in the `jq` command line tool for further inspection, e.g.
     *
     * {{{jq '.rows[0].elements[0].distance' input.json}}}
     */
    def fromJson[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.decodeJson(json)
  }
}
