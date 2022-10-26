val l = List()
val add = l ::: List("more")

"We All Live In A Yellow ..".split(" ").toList match {
  case Nil => "Empty"
  case head :: tail =>
    println(s"**${head}")
    println(tail)
    println(tail.mkString(" "))
}