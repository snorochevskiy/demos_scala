package basics

import scalaz._, Scalaz._

object TypeClass_Monad extends App {
  1.some >>= {a=>Some(a+1)} // Some(2)
  1.some >>= {a=> Monad[Option].point(a+1)} // Some(2)
}
