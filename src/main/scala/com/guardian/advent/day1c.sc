import scala.io.Source
import scala.util.Try

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day1a.txt").getLines().toList

def stringToNumber(s: String): Option[Int] = Try {
  val toNumerify = s.length match {
    case 1 => s"$s$s"
    case 2 => s
    case _ => s"${s.head}${s.last}"
  }
  toNumerify.toInt
}.toOption

val stringNumbers = List(
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four" , "4"),
  ("five" , "5"),
  ("six" , "6"),
  ("seven" , "7"),
  ("eight" , "8"),
  ("nine" , "9")
)

val s = "eightwothree"


/*
input
  .map{ s =>
    println(s)

    stringNumbers.foldLeft(s) { case(transforming, (numberAsString, numberAsChar)) => transforming.replaceAll(numberAsString, numberAsChar)} }
  .map{ s =>


    println(s)
    s.filter{ c => c.isDigit }
  }
  .flatMap {s => stringToNumber(s) }
  .foldRight(0) { case(total, callibration) => total + callibration }
*/
