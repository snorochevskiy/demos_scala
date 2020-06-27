package basics

import scalaz._, Scalaz._

object TypeClass_WriterT  extends App {

  type WriterOp[W, A] = WriterT[W, Option, A]

  def writerOp[W,A](o: Option[(W,A)]): WriterOp[W,A] = WriterT.writerT[Option, W, A](o)

  {
    val w1: WriterOp[String,Int] = writerOp(Option("one" -> 1)) // WriterT(Some((one,1)))
    val w2 = w1.flatMap(_ => writerOp(Some("two" -> 2)))        // WriterT(Some((onetwo,2)))
    val w3 = w2.flatMap(_ => writerOp[String,Int](None))        // WriterT(None)
    val w4 = w3.flatMap(_ => writerOp(Some("four" -> 4)))       // WriterT(None)
  }

  type WriterList[W,A] = WriterT[W,List,A]
  def writerList[W,A](l:List[(W,A)]): WriterList[W,A] = WriterT.writerT[List, W, A](l)

  {
    val w1 = writerList(List("one"->1))                              // WriterT(List((one,1)))
    val w2 = w1.flatMap(_ => writerList(List("two"->2)))             // WriterT(List((onetwo,2)))
    val w3 = w2.flatMap(_ => writerList(List("tree"->3, "four"->4))) // WriterT(List((onetwotree,3), (onetwofour,4)))
    val w4 = w3.flatMap(_ => writerList(List("five"->5)))            // WriterT(List((onetwotreefive,5), (onetwofourfive,5)))
    val w5 = w4.flatMap(_ => writerList[String,Int](Nil))            // WriterT(List())
  }
}
