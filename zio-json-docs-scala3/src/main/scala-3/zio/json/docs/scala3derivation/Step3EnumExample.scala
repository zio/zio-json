package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — Scala 3 enums: plain strings, wrapper objects and a discriminator field.
 */
object Step3EnumExample {

  // #region simple-enum
  enum Status derives JsonCodec {
    case Active, Suspended, Closed
  }
  // #endregion

  // #region enum-with-parameters
  enum PaymentMethod derives JsonCodec {
    case Card(last4: String)
    case Transfer(iban: String)
    case Cash
  }
  // #endregion

  // #region discriminator
  @jsonDiscriminator("type")
  enum Event derives JsonCodec {
    case Created(id: Long)
    case Deleted(id: Long)
    case Purged
  }
  // #endregion

  // #region hints
  enum Priority derives JsonCodec {
    @jsonHint("p1") case Urgent
    @jsonHint("p3") case Normal
  }
  // #endregion

  // #region as-objects
  object legacy {
    given JsonCodecConfiguration = JsonCodecConfiguration(enumValuesAsStrings = false)

    enum Level derives JsonCodec {
      case Low, High
    }
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Status encoded: " + (Status.Suspended: Status).toJson)
    println("Status decoded: " + "\"Closed\"".fromJson[Status])
    println("Status unknown: " + "\"Deleted\"".fromJson[Status])
    println("Card encoded: " + (PaymentMethod.Card("4242"): PaymentMethod).toJson)
    println("Cash encoded: " + (PaymentMethod.Cash: PaymentMethod).toJson)
    println("Created encoded: " + (Event.Created(7L): Event).toJson)
    println("Purged encoded: " + (Event.Purged: Event).toJson)
    println("Deleted decoded: " + """{"id":7,"type":"Deleted"}""".fromJson[Event])
    println("Priority encoded: " + (Priority.Urgent: Priority).toJson)
    println("Level encoded: " + (legacy.Level.High: legacy.Level).toJson)
  }
}
