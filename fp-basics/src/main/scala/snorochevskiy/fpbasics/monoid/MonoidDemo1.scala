package snorochevskiy.fpbasics.monoid

object MonoidDemo1 {

  type Input = Int

  def getMaybeNum(a: Input): Option[Int] = if (a % 3 == 0) None else Some(a)

  def main(args: Array[String]): Unit = {
    val result = plainOldApproach(List(1,2,3,4))
    println(result)
  }

  def plainOldApproach(inputs: List[Input]): Option[Int] =
    inputs
      .map(a => getMaybeNum(a))
      .reduce{ (accumOp: Option[Int], elemOp: Option[Int]) =>
        (accumOp, elemOp) match {
          case (Some(accum), Some(elem)) => Some(accum + elem)
          case (Some(accum), None)       => Some(accum)
          case (None, Some(elem))        => Some(elem)
          case _                         => None
        }
      }

}


