package snorochevskiy.cats.effects

import cats.effect.{IO, Resource}
import cats.syntax.all._
import java.io.{File, _}

object IoDemo {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    } { inStream =>
      IO(inStream.close()).handleErrorWith(_ => IO.unit)
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    } { outStream =>
      IO(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream  <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total  <- transmit(origin, destination, buffer, 0L)
    } yield total

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO{origin.read(buffer, 0, buffer.length)}
      count  <- if (amount > -1)
        IO(destination.write(buffer, 0, amount)) >>
          transmit(origin, destination, buffer, acc + amount)
      else
        IO.pure(acc)
    } yield count

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination)
      .use { case (in, out) => transfer(in, out) }

  def main(args: Array[String]): Unit = {
    val src = new File("/home/stas/test_src.txt")
    val dst = new File("/home/stas/test_dst.txt")
    val computation = for {
      count <- copy(src, dst)
      _     <- IO(println(s"$count bytes copied from ${src.getPath} to ${dst.getPath}"))
    } yield ()
    computation.unsafeRunSync()
  }

}
