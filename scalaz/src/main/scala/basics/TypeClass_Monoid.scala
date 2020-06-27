package basics

import scalaz._, Scalaz._

object TypeClass_Monoid extends App {

  List(1,2,3) |+| List(4) // List(1, 2, 3, 4)
  implicitly[Monoid[List[Int]]].zero // List()

  "abc" |+| "def" // "abcdef"
  implicitly[Monoid[String]].zero // ""

  5 |+| 4 // 9
  implicitly[Monoid[Int]].zero // 0

  Option(1) |+| Option(2) // Some(3)
  implicitly[Monoid[Option[Int]]].zero // None

  List(1,2,3).foldLeft(implicitly[Monoid[Int]].zero)(_ |+| _) // 6

  implicitly[Monoid[Int]].zero // 0
  implicitly[Monoid[Int @@ Tags.Multiplication]].zero // 1

  10 |+| 2 // 12
  Tags.Multiplication(10) |+| Tags.Multiplication(2) // 20


  implicitly[Monoid[Boolean @@ Tags.Disjunction]].zero // false
  implicitly[Monoid[Boolean @@ Tags.Conjunction]].zero // true
  Tags.Disjunction(true) |+| Tags.Disjunction(false) // true
  Tags.Conjunction(true) |+| Tags.Conjunction(false) // false

}
