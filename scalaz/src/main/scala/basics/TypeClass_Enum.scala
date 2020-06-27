package basics

import scalaz._, Scalaz._

object TypeClass_Enum extends App {

  val minInt = implicitly[Enum[Int]].min // Some(-2147483648)
  val maxInt = implicitly[Enum[Int]].max // Some(2147483647)
  implicitly[Enum[Int]].succ(minInt.get) // Some(-2147483647)
  minInt.get.succ                        // Some(-2147483647)

  implicitly[Enum[Boolean]].min // Some(false)
  implicitly[Enum[Boolean]].max // Some(true)

  implicitly[Enum[Unit]].max // Some(())
  implicitly[Enum[Unit]].max // Some(())

  def takeFirstElements[T](num: Int)(implicit ev: Enum[T]): List[T] =
    (1 to num).foldLeft(List(ev.min.get)){ (lst, _) => lst.head.succ :: lst }
      .reverse

  takeFirstElements[Byte](5) // List(-128,-127,-126,-125,-124,-123)

}

