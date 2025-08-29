import scala.util.Try

val l = "1234567".flatMap(
  x => Try(x.toInt).toOption
).toList