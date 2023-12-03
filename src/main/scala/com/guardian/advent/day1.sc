import scala.io.Source
import scala.util.Try

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day1.txt").getLines().toList

def stringToNumber(s: String): Option[Int] = Try {
  val toNumerify = s.length match {
    case 1 => s"$s$s"
    case 2 => s
    case _ => s"${s.head}${s.last}"
  }
  toNumerify.toInt
}.toOption

input
  .map{ s => s.filter(c => c.isDigit ) }
  .flatMap(stringToNumber(_) )
  .foldRight(0){ case(total, callibration) => total + callibration}