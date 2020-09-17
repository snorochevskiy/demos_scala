package snorochevskiy.fpbasics.semigroup

object MySemigroups {
  implicit val numMapSemigroup = new Semigroup[Map[String,Long]] {
    def combine(m1: Map[String,Long], m2: Map[String,Long]): Map[String,Long] =
      m1.toSeq.++(m2.toSeq)
        .groupBy(_._1)
        .view.mapValues(_.map(_._2).sum).toMap
  }
}



object SemigroupDemo {

  def main(args: Array[String]): Unit = {
    import MySemigroups._
    val m1 = Map("node1" -> 1L, "node2" -> 1L)
    val m2 = Map("node2" -> 2L)
    println(m1 |+| m2) // Map(node1 -> 1, node2 -> 3)
  }

}
