package basics

import scalaz._, Scalaz._
object TypeClass_MonadPlus extends App {

  val mpOp = implicitly[MonadPlus[Option]]
  mpOp.empty // None

  1.some <+> 2.some // Some(1)
  None <+> 2.some// Some(2)
  List(1,2,3) <+> List(4,5,6) // List(1, 2, 3, 4, 5, 6)


  (1 |-> 10) filter { _ % 2 == 0 } // [2,4,6,8,10]

}
