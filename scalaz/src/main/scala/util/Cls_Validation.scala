package util

import scalaz._, Scalaz._

object Cls_Validation extends App {
  val v1 = 5.success[String]
  println(v1) // Success(5)

  val v2 = (
    "<EVENT_1=OK>".success[String] |@|
      "<EVENT_2=FAIL>".failure[String] |@|
      "<EVENT_3=FAIL>".failure[String]
    ) {_ + _ + _}
  println(v2) // Failure(EVENT_2=FAILEVENT_3=FAIL)

  val v3 = (
    "<EVENT_1=OK>".successNel[String] |@|
      "<EVENT_2=FAIL>".failureNel[String] |@|
      "<EVENT_3=FAIL>".failureNel[String]
    ) {_ + _ + _}
  println(v3) // Failure(NonEmpty[<EVENT_2=FAIL>,<EVENT_3=FAIL>])
}
