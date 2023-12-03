import scala.io.Source
import scala.util.Try

implicit class RichBoolean(val b: Boolean) {def option[A](a: => A): Option[A] = if(b) Some(a) else None
}

val stringNumbers = Map(
	("one" , '1'),
	("two" , '2'),
	("three" , '3'),
	("four" , '4'),
	("five" , '5'),
	("six" , '6'),
	("seven" , '7'),
	("eight" , '8'),
	("nine" , '9')
)

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day1b.txt").getLines().toList

val a = input.flatMap{ line  =>
  val digitIndexes = line.zipWithIndex.toList.flatMap { case (c, index) =>
    c.isDigit.option(((c, index)))
  }

  val numbersForCalibration = stringNumbers.foldLeft(digitIndexes) {case(indexes, (numberAsString, numberAsChar) ) =>
    val r = s"${numberAsString}".r
    indexes ::: r.findAllMatchIn(line).map{ indexOfDigitString => (numberAsChar, indexOfDigitString.start) }.toList
  }.sortBy{ case (_, index) => index}
  .map{ case (numberAsChar, _) => numberAsChar }

  Try{
    val toNumerify = numbersForCalibration match {
      case List(x) => s"$x$x"
      case List(x, y) => s"$x$y"
      case _ => s"${numbersForCalibration.head}${numbersForCalibration.reverse.head}"
    }
    toNumerify.toInt
  }.toOption
}
a.foldRight(0){case(a, b) => a + b}

