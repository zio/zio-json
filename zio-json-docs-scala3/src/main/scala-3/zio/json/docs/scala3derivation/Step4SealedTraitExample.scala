package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — sealed traits and nested sealed families.
 */
object Step4SealedTraitExample {

  // #region sealed-trait
  sealed trait Shape derives JsonCodec
  final case class Circle(radius: Double)                 extends Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  // #endregion

  // #region nested
  sealed trait Vehicle derives JsonCodec
  sealed trait Motorized                 extends Vehicle
  final case class Car(seats: Int)       extends Motorized
  final case class Truck(payload: Double) extends Motorized
  final case class Bicycle(gears: Int)   extends Vehicle
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Circle encoded: " + (Circle(1.5): Shape).toJson)
    println("Rectangle decoded: " + """{"Rectangle":{"width":2,"height":3}}""".fromJson[Shape])
    println("Car encoded: " + (Car(4): Vehicle).toJson)
    println("Bicycle decoded: " + """{"Bicycle":{"gears":21}}""".fromJson[Vehicle])
    println("Motorized hint: " + """{"Motorized":{"Car":{"seats":4}}}""".fromJson[Vehicle])
  }
}
