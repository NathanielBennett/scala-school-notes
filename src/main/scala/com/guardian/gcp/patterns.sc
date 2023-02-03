val chDirPattern = """^\$\scd\s(\S+)$""".r
val dirPattern = """^dir\s(\S+)$""".r
val filePattern = """^(\d+)\s(\S+)$""".r

val s = "13454656 slobby"
s match {
  case chDirPattern(name) => println(s"$name")
  case dirPattern(name) =>  println(name)
  case filePattern(name, size) => print(s"$name:$size")
}


