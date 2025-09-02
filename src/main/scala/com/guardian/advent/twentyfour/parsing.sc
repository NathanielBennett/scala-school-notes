val l = List(
  List(1,2,3),
  List(3,4,5),
  List(),
  List(7)
)

val tp = l.flatMap{ case ks => ks.headOption.map {h => (ks, h)} }
tp.map { case (a, _) => a}