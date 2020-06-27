package util

import scalaz._, Scalaz._


object Ops_Either extends App {

  val e1: Either[String,Int] =
    Right[String,Int](5)

  val e2: String Either Int =
    Right(5)

  val e3: String \/ Int =
    5.right

  val incSome = Kleisli{ (a:Int) => Option(a+1) }
  val mul2Some = Kleisli{ (a:Int) => Option(a*2) }
  5.some >>= incSome <=< mul2Some // Some(11)

}
