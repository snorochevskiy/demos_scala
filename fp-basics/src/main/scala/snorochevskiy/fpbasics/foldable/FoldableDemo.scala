package snorochevskiy.fpbasics.foldable

import snorochevskiy.fpbasics.monoid.Monoid

object FoldableDemo {
  type HomoTuple3[A] = (A,A,A)

  implicit def toHomoTuple3Foldable[A]: Foldable[HomoTuple3,A] = new Foldable[HomoTuple3,A] {
    override def foldLeft[B](t: HomoTuple3[A])(initial: B)(op: (B, A) => B): B =
      op(op(op(initial, t._1), t._2), t._3)
  }

  implicit val monoidInstance: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def combine(a1: Int, a2: Int): Int = a1 + a2
  }

  def main(args: Array[String]): Unit = {
    val tup: HomoTuple3[Int] = (1,2,3)

    val foldLeftRes = tup.fl(5)(_+_)
    println(foldLeftRes) // 11

    val reduceMonoidRes = tup.rm()
    println(reduceMonoidRes) // 6
  }
}
