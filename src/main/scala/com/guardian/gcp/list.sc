val m = Map(
  1 -> List("One", "Two", "Theee"),
  2 -> List("Four", "Five", "Six")
)

val updates = List((1, "seven"), (2, "nine"), (3, "eleven"))

val flattened  = m.toList.flatMap {
  case(key, sList) =>
    val t = sList.map{ s => (key, s) }
    println(t)
    t
}

def transform(tuples: List[(Int, String)], map: Map[Int, List[String]] = Map.empty ): Map[Int, List[String]] = {
  val newMap = tuples.foldLeft(map) {
    case (m, tuple) =>
      val (key, value) = tuple
      val nextList = m.get(key).map{ vs => value :: vs }.getOrElse(List(value))
      m + (key -> nextList)
  }
  newMap.map{ case (key, list) => (key, list.reverse) }
}

val a = transform(flattened)
val al = a.toList
val b = transform(updates, a)
val bl = b.toList