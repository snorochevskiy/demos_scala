package snorochevskiy.fpbasics

package object functor {

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A=>B): F[B]
    def lift[A,B](f: A=>B): F[A]=>F[B] = map(_)(f)
  }

  object Functor {
    def apply[F[_]]()(implicit functorImpl: Functor[F]): Functor[F] = functorImpl
  }

  implicit class FunctorOps[F[_],A](val self: F[A]) extends AnyVal {
    def map[B](fa: A=>B)(implicit functorImpl: Functor[F]): F[B] = functorImpl.map(self)(fa)
  }

}
