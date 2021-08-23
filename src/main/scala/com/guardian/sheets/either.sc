val l = List(1,2,3,4)
val totalList = l.foldLeft(0) { case(acc, n) => acc + n }
val totalList2 = l.foldLeft(0) {  _ + _ }
val totalList4 = l.fold(0) { _ + _}

val l2 = 5 :: l

val allTheNames = l.foldLeft(List[String]()){ case (l, n) =>
  println(s"l: $l, n: $n")
  s"name: $n" :: l
}
