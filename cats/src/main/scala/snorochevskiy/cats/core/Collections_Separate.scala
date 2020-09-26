package snorochevskiy.cats.core

import cats.implicits._

object Collections_Separate {

  def main(args: Array[String]): Unit = {

    val list = List(
      Left("Error1"),
      Right(5),
      Left("Error1"),
      Right(10)
    )

    val (errs, nums): (List[String], List[Int]) = list.separate
  }
}
