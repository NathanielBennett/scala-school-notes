import scala.io.Source

val input = Source.fromFile(s"${System.getProperty("user.home")}/tuning.txt").mkString

def findCode(messageBody: List[Char], index: Int = 0) : Int = {
   val test = messageBody.take(4)
   if (test.size == test.toSet.size) index
   else findCode(messageBody.tail, index + 1)
}

val startOfCode = findCode(input.toList)

