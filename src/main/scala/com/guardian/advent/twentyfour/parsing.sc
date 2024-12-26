val l = List(52, 77, 83)
val idle = l.to(LazyList)

val p =  idle.permutations
val ls = p.toList

val t = ls.headOption.foreach( s => println(s.head) )

val ll = LazyList()
val s = LazyList.empty


