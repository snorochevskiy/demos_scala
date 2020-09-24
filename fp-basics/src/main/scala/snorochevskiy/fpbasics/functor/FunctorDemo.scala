package snorochevskiy.fpbasics.functor

object FunctorDemo {

  type HomoTuple3[A] = (A,A,A)

  implicit def toHomoTuple3Functor: Functor[HomoTuple3] = new Functor[HomoTuple3] {
    override def map[A, B](t: HomoTuple3[A])(f: A => B): HomoTuple3[B] =
      (f(t._1), f(t._2), f(t._3))
  }

  def main(args: Array[String]): Unit = {
    val t: HomoTuple3[Int] = (1, 2, 3)
    val res1 = t.map(_ * 2)
    println(res1) // (2, 4, 6)

    val fa: Int=>Int = (_: Int) * 2
    val ffa: HomoTuple3[Int]=>HomoTuple3[Int] = Functor[HomoTuple3].lift(fa)
    val res2 = ffa(t)
    println(res2) // (2, 4, 6)
  }
}
