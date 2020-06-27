package basics

import scalaz._, Scalaz._
object TypeClass_Iso extends App {

  class FullName(val firstName: String, val lastName: String)

  import Isomorphism.<=>

  val isoFullName = new (String <=> FullName) {
    val to: String=>FullName = { str =>
      val Array(firstName, secondName) = str.split("\\s")
      new FullName(firstName, secondName)
    }
    val from: FullName=>String = n =>
      s"${n.firstName} ${n.lastName}"
  }

  implicit val isoEq = Equal.fromIso[FullName, String](isoFullName.flip)
  new FullName("John", "Doe") === new FullName("John", "Doe") // true
}
