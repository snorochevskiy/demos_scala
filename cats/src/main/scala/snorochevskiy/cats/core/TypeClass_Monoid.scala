package snorochevskiy.cats.core


import cats.kernel.Semigroup

object TypeClass_Monoid {
  def main(args: Array[String]): Unit = {

    import cats.syntax.semigroup._
    import cats.instances.option._

    import cats.instances.int._
    Semigroup[Option[Int]].combine(Option(1), Option(2)) // Some(3)
    Option(1) |+| Option(3)
    (None: Option[Int]) |+| Option(2) // Some(2)

    import cats.implicits.catsSyntaxOptionId
    1.some |+| 2.some // Some(3)

    import cats.instances.string._
    "aaa" |+| "bbb" // "aaabbb"
    Option("aaa") |+| Option("bbb") // Some("aaabbb")

    import cats.implicits.catsSyntaxTuple2Semigroupal
    (Option(5), Option(2)) mapN (_ * _) // Some(10)
    (Option(5), None: Option[Int]) mapN (_ * _) // None


  }
}
