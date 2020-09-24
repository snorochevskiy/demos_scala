package snorochevskiy.fpbasics.monad

import snorochevskiy.fpbasics.functor._

object MonadDemo {

  implicit def toFunction0Monad: Monad[Function0] with Functor[Function0] = new Monad[Function0] with Functor[Function0] {
    override def flatMap[A, B](f0: ()=>A)(f: A=>()=>B): ()=>B = f(f0())
    override def pure[A](a: A): ()=>A = ()=>a
    override def map[A, B](f0: ()=>A)(f: A => B): ()=>B = ()=>f(f0())
  }

  def main(args: Array[String]): Unit = {
    val func1 = Monad[Function0].pure(5)
      .flatMap(a => ()=> a + 10)
    println(func1()) // 15

    val func2 = for {
      a <- Monad[Function0].pure(5)
      b <- ()=> a + 10
    } yield b
    println(func2()) // 15
  }

}
