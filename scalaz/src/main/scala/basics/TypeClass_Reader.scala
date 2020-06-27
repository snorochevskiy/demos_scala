package basics


import scalaz.{StateT, _}
import Scalaz._
object TypeClass_Reader extends App {

  {
    val myFunc: Int => Int = for {
      a <- (_: Int) * 2
      b <- (_: Int) + 10
    } yield a + b

    myFunc(3) // 19
  }

  {
    def introduce(step: String): Reader[String, String] = Reader {step + " I am " + _}
    def introduceReader: Reader[String, (String, String, String)] = for {
        a <- introduce("(1)")
        b <- introduce("(2)") >=> Reader { _ + "ik"}
        c <- introduce("(3)")
      } yield (a, b, c)
    introduceReader("Stas") // ((1) I am Stas,(2) I am Stasik,(3) I am Stas)
  }

}
