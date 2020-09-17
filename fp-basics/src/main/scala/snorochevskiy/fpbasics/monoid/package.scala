package snorochevskiy.fpbasics

import snorochevskiy.fpbasics.semigroup.Semigroup

package object monoid {

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

  implicit class MonoidOps[A](val self: A) {
    def |+|(other: A)(implicit monoidInstance: Monoid[A]): A =
      monoidInstance.combine(self, other)
  }
}
