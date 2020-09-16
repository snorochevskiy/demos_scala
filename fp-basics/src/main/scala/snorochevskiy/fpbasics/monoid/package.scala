package snorochevskiy.fpbasics

package object monoid {

  trait Monoid[A] {
    def zero: A
    def mappend(a1: A, a2: A): A
  }

  implicit class MonoidOps[A: Monoid](val self: A) {
    def |+|(other: A): A = {
      val monoidInstance: Monoid[A] = implicitly[Monoid[A]]
      monoidInstance.mappend(self, other)
    }
  }

}
