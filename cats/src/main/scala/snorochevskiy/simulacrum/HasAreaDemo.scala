package snorochevskiy.simulacrum

case class Square(width: Double)

object HasAreaDemo {
  implicit val squareHasArea = new HasArea[Square] {
    override def calcArea(a: Square): Double = a.width * a.width
  }

  def main(args: Array[String]): Unit = {
    import HasArea.ops._
    val s = Square(5.0)
    println(s.area)
  }
}
