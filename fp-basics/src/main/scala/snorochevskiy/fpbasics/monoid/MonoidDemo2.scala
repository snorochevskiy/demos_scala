package snorochevskiy.fpbasics.monoid

object MyMonoids {
  implicit val opIntMonoid = new Monoid[Option[Int]] {
    def zero: Option[Int] = None
    def mappend(a1: Option[Int], a2: Option[Int]): Option[Int] =
      (a1, a2) match {
        case (Some(accum), Some(elem)) => Some(accum + elem)
        case (Some(accum), None)       => Some(accum)
        case (None, Some(elem))        => Some(elem)
        case _                         => None
      }
  }
}

object MonoidDemo2 {

  type Input = Int

  def getMaybeNum(a: Input): Option[Int] = if (a % 3 == 0) None else Some(a)

  def main(args: Array[String]): Unit = {
    val result = withMonoidApproach(List(1,2,3,4))
    println(result)
  }

  import MyMonoids._
  def withMonoidApproach(inputs: List[Input]): Option[Int] =
    inputs
      .map(a => getMaybeNum(a))
      .reduce{ (accumOp: Option[Int], elemOp: Option[Int]) => accumOp |+| elemOp }

}


