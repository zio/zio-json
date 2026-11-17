package zio.json.docs.decoding

import zio.json.*

/**
 * Decoding how-to — string-literal union types on Scala 3.
 */
object UnionTypesExample {

  // #region union
  type AppleOrBanana = "Apple" | "Banana"

  val appleOrBanana: AppleOrBanana = "Apple"

  val encoded = appleOrBanana.toJson                  // "Apple", with JSON quotes
  val valid   = """"Apple"""".fromJson[AppleOrBanana] // the input is the JSON string "Apple"
  val invalid = """"Pear"""".fromJson[AppleOrBanana]
  // #endregion

  def main(args: Array[String]): Unit = {
    println("encoded: " + encoded)
    println("valid: " + valid)
    println("invalid: " + invalid)
  }
}
