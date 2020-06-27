package basics

import scalaz._, Scalaz._

object TypeClass_Functor extends App {

  val listFunctor = implicitly[Functor[List]]
  listFunctor.map(List(1, 2, 3)){_ + 1} // List(2, 3, 4)


  (1, 1, 1) map (_ + 1) // (1,1,2)

  val composedFunc = ((x: Int) => x + 5) map {_ * 2}
  composedFunc(1) // 12

  val incElems = Functor[List].lift {(_: Int) + 1}
  incElems(List(1,2,3)) // List(2, 3, 4)

}
