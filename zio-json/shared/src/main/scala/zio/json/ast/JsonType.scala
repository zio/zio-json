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

/**
 * A runtime description of one kind of JSON value, used by a `JsonCursor` to check the type of a node.
 *
 * `JsonType.Null`, `JsonType.Bool`, `JsonType.Obj`, `JsonType.Arr`, `JsonType.Str` and `JsonType.Num` are its only
 * instances.
 */
sealed trait JsonType[A] {
  /**
   * Returns `json` as the expected kind of value, or `Left` with `expected <kind>` when it has a different kind.
   */
  def get(json: Json): Either[String, A]
}

/**
 * The instances of [[JsonType]].
 */
object JsonType {
  private[json] val rightNull: Either[String, Json.Null] = new Right(Json.Null)
  private val expectedNull: Left[String, Nothing]        = new Left("expected null")
  private val expectedBool: Left[String, Nothing]        = new Left("expected boolean")
  private val expectedObject: Left[String, Nothing]      = new Left("expected object")
  private val expectedArray: Left[String, Nothing]       = new Left("expected array")
  private val expectedString: Left[String, Nothing]      = new Left("expected string")
  private val expectedNumber: Left[String, Nothing]      = new Left("expected number")

  /**
   * The type of `Json.Null`. Its `get` fails with `expected null` for any other value.
   */
  case object Null extends JsonType[Json.Null] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Null] =
      json match {
        case _: Json.Null.type => rightNull
        case _                 => expectedNull
      }
  }

  /**
   * The type of `Json.Bool`. Its `get` fails with `expected boolean` for any other value.
   */
  case object Bool extends JsonType[Json.Bool] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Bool] =
      json match {
        case x: Json.Bool => new Right(x)
        case _            => expectedBool
      }
  }

  /**
   * The type of `Json.Obj`. Its `get` fails with `expected object` for any other value.
   */
  case object Obj extends JsonType[Json.Obj] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Obj] =
      json match {
        case x: Json.Obj => new Right(x)
        case _           => expectedObject
      }
  }

  /**
   * The type of `Json.Arr`. Its `get` fails with `expected array` for any other value.
   */
  case object Arr extends JsonType[Json.Arr] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Arr] =
      json match {
        case x: Json.Arr => new Right(x)
        case _           => expectedArray
      }
  }

  /**
   * The type of `Json.Str`. Its `get` fails with `expected string` for any other value.
   */
  case object Str extends JsonType[Json.Str] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Str] =
      json match {
        case x: Json.Str => new Right(x)
        case _           => expectedString
      }
  }

  /**
   * The type of `Json.Num`. Its `get` fails with `expected number` for any other value.
   */
  case object Num extends JsonType[Json.Num] {
    /**
     * Returns `json` as the expected kind of value, or `Left` with an `expected ...` message when it has a different kind.
     */
    def get(json: Json): Either[String, Json.Num] =
      json match {
        case x: Json.Num => new Right(x)
        case _           => expectedNumber
      }
  }
}
