import scala.util.Try

def getTry(devideBy: Int) : Try[Int] = Try{ 4 / devideBy }

def wrapTryToFold(devideBy: Int): Option[String] = getTry(devideBy).fold({
  case t: Throwable =>
    //println(s"Caught throwable ${t.getMessage}")
    None
  },
  {
    case n: Int => Option(s"Result: $n")
  }
)

val h = wrapTryToFold(0)
val g = wrapTryToFold(2)
