package basics

import scalaz._, Scalaz._

object Types_Tags extends App {

  def usingTag() = {
    trait Name

    def makeName(name: String): String @@ Name = Tag[String, Name](name)

    val name = makeName("Jonh") // at compile String@@Name
    val str = Tag.unwrap(name) // java.lang.String

    println(name.getClass.getName) // java.lang.String
  }
  usingTag()

  def wiring() = {
    // Tags hierarchy dor double values: kilograms and meters
    sealed trait NumType
    trait Kilogram extends NumType
    trait Meter extends NumType

    // Constructing functions, just for convenience
    def kilogram(a: Double): Double @@ Kilogram = Tag[Double, Kilogram](a)
    def meter(a: Double): Double @@ Meter = Tag[Double, Meter](a)

    // Type class that provides the name of a measurement by it's tag
    trait NameProvider[NumType] {
      def name(): String
    }
    implicit object KilogramNameProvider extends NameProvider[Kilogram] {
      def name(): String = "Kilogram"
    }
    implicit object MeterNameProvider extends NameProvider[Meter] {
      def name(): String = "Meter"
    }

    // Function that formats numbers based on their tags
    def format[N<:NumType](n: Double @@ N)(implicit np: NameProvider[N]) = {
      np.name() + ": " + n
    }

    format(kilogram(20.0)) // Kilogram: 20.0
    format(meter(50.0)) // Meter: 50.0
  }

}
