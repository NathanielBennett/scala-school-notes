def intToString(i: Int): String = s"Number $i"

val n = intToString(10)
val m = intToString(2)

def basicOptionIntMap(maybeInt: Option[Int]) : Option[String] = maybeInt match {
  case Some(n) => Some(intToString(n))
  case None => None
}

val maybeOne = Some(1)
val maybeTen = Some(10)

val basicMapOne = basicOptionIntMap(maybeOne)
val basicMapTen = basicOptionIntMap(maybeTen)
val none = basicOptionIntMap(None)

def makeXString(x: Int): String = (0 to x).foldLeft(""){ case (acc, _) => s"${acc}X" }

makeXString(5)
makeXString(10)

def optionIntMap(maybeInt: Option[Int])(convertFunction: Int => String) : Option[String] =
  maybeInt match {
    case Some(n) => Some(convertFunction(n))
    case None => None
  }

val numberString = optionIntMap(Some(10))(intToString)
val xString = optionIntMap(Some(10))(makeXString)

val numberStringInline = optionIntMap(Some(10)) {
  i => s"Number $i"
}

val xStringInline = optionIntMap(Some(10)) {
  i => (0 to i).foldLeft(""){ case(acc, _) => s"${acc}X"}
}

def optionMap[A, B](maybeA: Option[A])(f: A => B): Option[B] = maybeA match {
  case Some(a) => Some(f(a))
  case None => None
}

val genericNumberString = optionMap(Some(10)) { i => s"Number $i" }
val genericXString = optionMap(Some(10)) { i => (0 to i).foldLeft(""){ case(acc, _) => s"${acc}X"} }

val optionUpercase = optionMap(Some("hello")){ s => s.toUpperCase }
val optionStringLength = optionMap(Some("hello")) { s => s.length }

