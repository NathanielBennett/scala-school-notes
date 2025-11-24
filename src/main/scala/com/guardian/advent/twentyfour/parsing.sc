val l = List(
  List(1,2),
  List(1,4),
  List(1,2,5),
  List(1,2,6),
  List(1,3),
  List(1,2,6)
)

val (_, all) = l.groupBy{ ls => ls.length }
  .minBy{ case (len, _ ) => len }

all.flatten.toSet.toList