package zio.json.docs.generictypes

import zio.json.*

/**
 * Derive codecs for generic types — the Scala 3 forms.
 */
object GenericTypesExample {

  // #region derives
  final case class Box[A](value: A) derives JsonCodec
  // #endregion

  // #region given
  final case class Page[A](items: List[A], total: Int)

  object Page {
    given [A: JsonEncoder: JsonDecoder]: JsonCodec[Page[A]] = DeriveJsonCodec.gen
  }
  // #endregion

  // #region separate
  final case class Delta[A](before: A, after: A)

  object Delta {
    given [A: JsonEncoder]: JsonEncoder[Delta[A]] = DeriveJsonEncoder.gen
    given [A: JsonDecoder]: JsonDecoder[Delta[A]] = DeriveJsonDecoder.gen
  }
  // #endregion

  // #region sealed
  sealed trait Result[+A] derives JsonCodec
  final case class Ok[A](value: A)             extends Result[A]
  final case class Failed[A](reason: String)   extends Result[A]
  case object Pending                          extends Result[Nothing]
  // #endregion

  // #region recursive
  final case class Tree[A](value: A, children: List[Tree[A]]) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Box: " + Box("x").toJson)
    println("Page: " + Page(List(1, 2), 2).toJson)
    println("Page decoded: " + """{"items":["a"],"total":1}""".fromJson[Page[String]])
    println("Delta: " + Delta(1, 2).toJson)
    println("Ok: " + (Ok(1): Result[Int]).toJson)
    println("Failed: " + (Failed[Int]("timeout"): Result[Int]).toJson)
    println("Pending: " + (Pending: Result[Int]).toJson)
    println("Result decoded: " + """{"Failed":{"reason":"timeout"}}""".fromJson[Result[Int]])
    println("Tree: " + Tree(1, List(Tree(2, Nil))).toJson)
    println("Tree decoded: " + """{"value":1,"children":[{"value":2,"children":[]}]}""".fromJson[Tree[Int]])
  }
}
