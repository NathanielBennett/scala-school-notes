import scala.util.{Failure, Success, Try}

def maybeProject(int: Int): Option[String] =
  if(int == 1) Some("Gotcha")
  else None


def tryOne(int: Int) : Try[String] = { Try {
    if (int == 1) throw new IllegalArgumentException(s"That is illegal $int")
    else s"String $int"
  } recoverWith {
    case t: Throwable =>
      println("There was an error. Innit")
      Failure(t)
  }

}

maybeProject(1)

val innit = (for{
  proj <- maybeProject(1)
  conf <- tryOne(1).toOption
} yield conf)
