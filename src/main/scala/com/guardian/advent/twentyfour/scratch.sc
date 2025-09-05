val l =  63774008693269L
val s = l.toString
s.splitAt(s.length / 2) match {
  case (stoneOne, stoneTwo) => List(stoneOne.toLong, stoneTwo.toLong)
}

