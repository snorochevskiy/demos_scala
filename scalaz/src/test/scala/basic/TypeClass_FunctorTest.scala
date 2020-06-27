package basic

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scalaz._, Scalaz._


class TypeClass_FunctorTest extends AnyWordSpec with Matchers {

  "Functor" must {
    "follow functor laws for List" in {

      import scalacheck.ScalazProperties._
      import scalacheck.ScalazArbitrary._
      import scalacheck.ScalaCheckBinding._
      functor.laws[List].check()
    }

    "fail for functor with side effect" in {
      sealed trait OptWithCounter[+A]
      case object NoneWithCounter extends OptWithCounter[Nothing]
      case class SomeWithCounter[A](c: Int, a: A) extends OptWithCounter[A]

      implicit def optWithCounterEqual[A]: Equal[OptWithCounter[A]] = Equal.equalA // natural equality
      implicit val optWithCounterFunctor = new Functor[OptWithCounter] {
        def map[A, B](fa: OptWithCounter[A])(f: A => B): OptWithCounter[B] = fa match {
          case NoneWithCounter => NoneWithCounter
          case SomeWithCounter(c, a) => SomeWithCounter(c + 1, f(a))
        }
      }

      import org.scalacheck.{Gen, Arbitrary}
      import scalacheck.ScalazArbitrary._
      import scalacheck.ScalaCheckBinding._

      implicit def COptionArbiterary[A](implicit a: Arbitrary[A]): Arbitrary[OptWithCounter[A]] =
        a map { a => (SomeWithCounter(0, a): OptWithCounter[A]) }

      import scalacheck.ScalazProperties._
      functor.laws[OptWithCounter].check()
    }
  }
}
