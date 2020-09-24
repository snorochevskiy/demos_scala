package snorochevskiy.fpbasics

import snorochevskiy.fpbasics.functor.Functor

package object monad {

  trait Monad[F[_]] { this: Functor[F] =>
    def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B]
    def pure[A](a: A): F[A]
  }

  implicit class MonadOps[M[_],A](val self: M[A]) extends AnyVal {
    def flatMap[B](f: A=>M[B])(implicit monadImpl: Monad[M]): M[B] =
      monadImpl.flatMap(self)(f)
  }

  object Monad {
    def apply[F[_]]()(implicit monadImpl: Monad[F]): Monad[F] = monadImpl
  }

}
