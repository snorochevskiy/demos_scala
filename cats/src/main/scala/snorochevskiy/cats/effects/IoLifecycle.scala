package snorochevskiy.cats.effects

import cats.effect.{IO, Resource}

object IoLifecycle {

  def main(args: Array[String]): Unit = {
    producerAndConsumer().use { case (p, c) =>
      IO{ c(p()) }
    }.unsafeRunSync()
  }

  def producer(): Resource[IO, Function0[String]] =
    Resource.make {
      println("creating producer")
      IO( () => { println("calling producer"); "1" } )
    } { f => IO{ println("closing producer") } }

  def consumer(): Resource[IO, Function[String,Unit]] =
    Resource.make {
      println("creating consumer")
      IO( {s: String => println("Calling consumer")} )
    } { f => IO{println("closing consumer")}}

  def producerAndConsumer(): Resource[IO, (Function0[String], Function[String,Unit])] =
    for {
      p <- producer()
      c <- consumer()
    } yield (p, c)
}
