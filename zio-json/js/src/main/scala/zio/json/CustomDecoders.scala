package zio.json.custom

import zio.json._

// Define the nested types
final case class Id[A](value: A)
final case class Person(name: String) extends AnyVal
final case class People(ids: Id[Person])

// Implement the JsonDecoder for Id
object Id {
  implicit def decoder[A: JsonDecoder]: JsonDecoder[Id[A]] = JsonDecoder[A].map(Id(_))
}

// Implement the JsonDecoder for Person
object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
}

// Implement the JsonDecoder for People
object People {
  implicit val decoderIds: JsonDecoder[Id[Person]] = Id.decoder[Person]
  implicit val decoder: JsonDecoder[People] = DeriveJsonDecoder.gen[People]
}
