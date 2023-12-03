import scala.util.Try

val line = "oneight"

val stringNumbers = List(
  ("one",  '1'),
  ("two", '2'),
  ("three", '3'),
  ("four" , '4'),
  ("five" , '5'),
  ("six" , '6'),
  ("seven" ,'7'),
  ("eight" , '8'),
  ("nine" , '9')
)

def listToInt(numbersForCalibration: List[Char]): Option[Int] = Try{
  val toNumerify = numbersForCalibration match {
    case List(x) => s"$x$x"
    case List(x, y) => s"$x$y"
    case _ => s"${numbersForCalibration.head}${numbersForCalibration.reverse.head}"
  }
  toNumerify.toInt
}.toOption

implicit class RichBoolean(val b: Boolean) {
  def option[A](a: => A): Option[A] = if(b) Some(a) else None
}

val digitIndexes = line.zipWithIndex.toList.flatMap { case (c, index) =>
  c.isDigit.option(((c, index)))
}

val numbersForCalibration = stringNumbers.foldLeft(digitIndexes) {case(indexes, (numberAsString, numberAsChar) ) =>
  val indexOfDigitAsString = line.indexOf(numberAsString)
  if(indexOfDigitAsString >= 0) {
    indexes ::: List((numberAsChar,indexOfDigitAsString))
  }
  else indexes
}

val sorted =  numbersForCalibration.sortBy{ case (_, index) => index}
val st =  sorted.map{ case (numberAsChar, _) => numberAsChar }
listToInt(st)
