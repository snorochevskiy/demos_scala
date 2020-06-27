package basics

import scalaz._, Scalaz._

object TypeClass_Applicative extends App {

  println(1.point[Option]) // Some(1)
  println(1.point[List]) // List(1)
  println({(_:Int)+1}.point[Option]) // Option[Int=>Int]

  val a = {(_:Int)+1}.point[Option]


  val opApplicative = implicitly[Applicative[Option]]
  val res: Option[Int] = opApplicative.ap2(Option(1), Option(2))({(_:Int)+(_:Int)}.point[Option])

  Option(1) <*> Option({(_:Int)+1}) // Some(2)
  Option(1) <*> { Option(2) <*> Option({(_:Int)+(_:Int)}.curried) } // Some(3)
  Option(1) <*> { Option(2) map {(_:Int)+(_:Int)}.curried } // Some(3)

  println( (Option(1) |@| Option(2)) {_+_})

  val liftedSum = Applicative[Option].lift2{(_:Int) + (_:Int)}
  liftedSum(Option(3), Option(2)) // Some(3)

}
