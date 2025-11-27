import scala.util.Try

val  prog = "1, 3, 4, 9, 2".split(", ").map{_.toByte}.toList

prog.indices.contains(6)

val ll = List(2,4,1,7,7,5,1,7,0,3,4,1,5,5,3,0)
ll.length