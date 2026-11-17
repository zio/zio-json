package zio.json.docs.scala3derivation

import scala.compiletime.testing.typeCheckErrors

import zio.json.*

/**
 * Derive codecs on Scala 3 — the compile errors derivation reports. The failing code is checked with `typeCheckErrors`,
 * so this file compiles and prints the messages.
 */
object Step8CompileErrorsExample {

  // #region missing-instance
  final case class Upload(name: String, file: java.io.File)
  // #endregion

  // #region missing-opaque
  object ids {
    opaque type AccountId = String
  }

  final case class Account(id: ids.AccountId)
  // #endregion

  def main(args: Array[String]): Unit = {
    typeCheckErrors("DeriveJsonCodec.gen[Upload]").foreach(e => println("Upload: " + e.message))
    typeCheckErrors("DeriveJsonEncoder.gen[Account]").foreach(e => println("Account: " + e.message))
  }
}
