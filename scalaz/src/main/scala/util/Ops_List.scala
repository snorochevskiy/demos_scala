package util

import scalaz._, Scalaz._

object Ops_List extends App {

  val l1 = List(1, 2, 3) filterM { x => List(true) }
  println(l1) // List(List(1, 2, 3))

  val l2 = List(1, 2, 3) filterM { x => List(false) }
  println(l2) // List(List())

  val l3 = List(1, 2) filterM { x => List(true, true) }
  println(l3)
  // List(List(1, 2), List(1, 2), List(1, 2), List(1, 2))

  val l5 = List(1, 2, 3) filterM { x => List(true,false) }
  println(l5)
  // List(List(1, 2, 3), List(1, 2), List(1, 3), List(1), List(2, 3), List(2), List(3), List())
}
