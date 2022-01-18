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
package zio.json.ast

sealed trait JsonType[A] {
  def get(json: Json): Either[String, A]
}

object JsonType {
  case object Null extends JsonType[Json.Null] {
    def get(json: Json): Either[String, Json.Null] =
      json match {
        case Json.Null => Right(Json.Null)
        case _         => Left("Expected null but found " + json)
      }
  }

  case object Bool extends JsonType[Json.Bool] {
    def get(json: Json): Either[String, Json.Bool] =
      json match {
        case x @ Json.Bool(_) => Right(x)
        case _                => Left("Expected boolean but found " + json)
      }
  }

  case object Obj extends JsonType[Json.Obj] {
    def get(json: Json): Either[String, Json.Obj] =
      json match {
        case x @ Json.Obj(_) => Right(x)
        case _               => Left("Expected object but found " + json)
      }
  }

  case object Arr extends JsonType[Json.Arr] {
    def get(json: Json): Either[String, Json.Arr] =
      json match {
        case x @ Json.Arr(_) => Right(x)
        case _               => Left("Expected array but found " + json)
      }
  }

  case object Str extends JsonType[Json.Str] {
    def get(json: Json): Either[String, Json.Str] =
      json match {
        case x @ Json.Str(_) => Right(x)
        case _               => Left("Expected string but found " + json)
      }
  }

  case object Num extends JsonType[Json.Num] {
    def get(json: Json): Either[String, Json.Num] =
      json match {
        case x @ Json.Num(_) => Right(x)
        case _               => Left("Expected number but found " + json)
      }
  }
}
