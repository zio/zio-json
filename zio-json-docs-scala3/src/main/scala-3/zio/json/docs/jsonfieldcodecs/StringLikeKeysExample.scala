package zio.json.docs.jsonfieldcodecs

import zio.json._

/**
 * JsonFieldEncoder and JsonFieldDecoder reference — Scala 3 key types that are subtypes of `String`.
 */
object StringLikeKeysExample {

  // #region union-keys
  type Env = "dev" | "prod"

  final case class Replicas(byEnv: Map[Env, Int]) derives JsonCodec
  // #endregion

  // #region enum-keys
  enum Region {
    case Eu, Us
  }

  object Region {
    given JsonFieldEncoder[Region] = JsonFieldEncoder[String].contramap(_.toString.toLowerCase)
    given JsonFieldDecoder[Region] = JsonFieldDecoder[String].mapOrFail { s =>
      Region.values.find(_.toString.toLowerCase == s).toRight(s"unknown region: $s")
    }
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("encode: " + Replicas(Map("dev" -> 1, "prod" -> 3)).toJson)
    println("decode: " + """{"byEnv":{"dev":1,"prod":3}}""".fromJson[Replicas])
    println("bad key: " + """{"byEnv":{"test":1}}""".fromJson[Replicas])
    println("enum encode: " + Map(Region.Eu -> 2).toJson)
    println("enum decode: " + """{"us":5}""".fromJson[Map[Region, Int]])
    println("enum bad: " + """{"asia":5}""".fromJson[Map[Region, Int]])
  }
}
