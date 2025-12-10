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
  private[json] val rightNull: Either[String, Json.Null] = new Right(Json.Null)
  private val expectedNull: Left[String, Nothing]        = new Left("expected null")
  private val expectedBool: Left[String, Nothing]        = new Left("expected boolean")
  private val expectedObject: Left[String, Nothing]      = new Left("expected object")
  private val expectedArray: Left[String, Nothing]       = new Left("expected array")
  private val expectedString: Left[String, Nothing]      = new Left("expected string")
  private val expectedNumber: Left[String, Nothing]      = new Left("expected number")

  case object Null extends JsonType[Json.Null] {
    def get(json: Json): Either[String, Json.Null] =
      json match {
        case _: Json.Null.type => rightNull
        case _                 => expectedNull
      }
  }

  case object Bool extends JsonType[Json.Bool] {
    def get(json: Json): Either[String, Json.Bool] =
      json match {
        case x: Json.Bool => new Right(x)
        case _            => expectedBool
      }
  }

  case object Obj extends JsonType[Json.Obj] {
    def get(json: Json): Either[String, Json.Obj] =
      json match {
        case x: Json.Obj => new Right(x)
        case _           => expectedObject
      }
  }

  case object Arr extends JsonType[Json.Arr] {
    def get(json: Json): Either[String, Json.Arr] =
      json match {
        case x: Json.Arr => new Right(x)
        case _           => expectedArray
      }
  }

  case object Str extends JsonType[Json.Str] {
    def get(json: Json): Either[String, Json.Str] =
      json match {
        case x: Json.Str => new Right(x)
        case _           => expectedString
      }
  }

  case object Num extends JsonType[Json.Num] {
    def get(json: Json): Either[String, Json.Num] =
      json match {
        case x: Json.Num => new Right(x)
        case _           => expectedNumber
      }
  }
}
