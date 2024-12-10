import scala.util.Try

val l = List(1,2, 3)
Try{ l match {
  case List(x, y) => (x,y)
}}.toOption