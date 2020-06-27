package basics

import scalaz.{Writer, _}
import Scalaz._

object TypeClass_Writer extends App {

  {
    val w2 = "one".tell
    println(w2) // WriterT((one,()))
  }

  {
    val w1 = List("a").tell // WriterT((List(a),()))
    val w2 = w1 :++> List("b") // WriterT((List(a, b),()))
    val w3 = w2.map(_=>1) // WriterT((List(a, b),1))
    val w4 = w3.flatMap(_+2 set List("c")) // WriterT((List(a, b, c),3))
  }

  {
    println(3.set("Smallish gang.")) // WriterT((Smallish gang.,3))
  }

  {
    val w1 = 3 set "three"
    println(w1) // WriterT((three,3))
    val w2 = w1 :++> "THREE"
    println(w2) // WriterT((threeTHREE,3))
  }

  {
    val w = for {
      _ <- Vector("Written text").tell
    } yield 5
    println(w) // WriterT((Vector(Written text),5))
  }

  {
    def trackNum(n:Int): Writer[List[String],Int] = n set List("Num:"+n.shows)
    val w = for (a <- trackNum(1); b <- trackNum(2)) yield a+b
    w.run // (List(Num:1, Num:2),3)
  }

}
