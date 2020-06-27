package basics

import scalaz._, Scalaz._

object TypeClass_Order extends App {

  println(5 lt 6) // true

  println(5 ?|? 4) // Ordering.GT
  println(5 ?|? 5) // Ordering.EQ
  println(5 ?|? 6) // Ordering.LT

  println(5 max 6) // 6

  // 5 ?|? 6.0 // required: Int, found: Double

  implicitly[Order[Int]].sort(5, 6).println
  implicitly[Order[Int]].sort(6, 5).println
}
