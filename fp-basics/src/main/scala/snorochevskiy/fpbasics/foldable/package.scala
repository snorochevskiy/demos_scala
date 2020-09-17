package snorochevskiy.fpbasics

import snorochevskiy.fpbasics.monoid.Monoid

package object foldable {

  trait Foldable[F[_], A] {
    def foldLeft[B](f: F[A])(initial: B)(op: (B, A) => B): B

    def reduceMonoid(f: F[A])(implicit monoidInstance: Monoid[A]): A =
      foldLeft(f)(monoidInstance.zero)(monoidInstance.combine)
  }

  implicit class FoldableOps[F[_],A](val a: F[A]) extends AnyVal {
    def fl[B](initial: B)(op: (B, A) => B)(implicit foldableInstance: Foldable[F,A]): B =
      foldableInstance.foldLeft(a)(initial)(op)

    def rm()(implicit foldableInstance: Foldable[F,A], monoidInstance: Monoid[A]): A =
      foldableInstance.reduceMonoid(a)
  }
}
