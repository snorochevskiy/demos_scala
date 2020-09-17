package snorochevskiy.fpbasics

package object semigroup {

  trait Semigroup[A] {
    def combine(a1: A, a2: A): A
  }

  implicit class SemigroupOps[A : Semigroup](val self: A) {
    def |+|(other: A)(implicit semigroupIml: Semigroup[A]): A =
      semigroupIml.combine(self, other)
  }

}
