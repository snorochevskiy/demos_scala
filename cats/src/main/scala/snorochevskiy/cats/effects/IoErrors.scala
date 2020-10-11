package snorochevskiy.cats.effects

import cats.effect.IO

object IoErrorsDemo {

  def main(args: Array[String]): Unit = {
    val calc = for {
      d1 <- IO(5)
      d2 <- IO(0)
      d3 <- IO(d1 / d2).handleErrorWith {
        case e: ArithmeticException => IO(0)
      }
    } yield d3
    println(calc.unsafeRunSync())
  }
}
