import scala.io.Source
import scala.util.Try

implicit class RichBoolean(val b: Boolean) extends AnyVal {
  final def option[A](a: => A): Option[A] = if (b) Some(a) else None
}

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day3test.txt").getLines().toList
case class EnginePart(char: Char, yPos: Int, xPos: Int) {

}


val allEngineParts = input.zipWithIndex.flatMap {
  case (line, yPos) => line.zipWithIndex.map { case (char, xPos) => EnginePart(char, yPos, xPos) }
}

def getAllNumbers(engineParts: List[EnginePart],
                  currentNumber: List[EnginePart] = List.empty,
                  acc: List[List[EnginePart]] = List.empty): List[List[EnginePart]] = engineParts match {
  case Nil => acc
  case head :: tail => head.char.isDigit match {
    case true => getAllNumbers(tail, currentNumber ::: List(head), acc)
    case false => currentNumber match {
      case List() => getAllNumbers(tail, currentNumber, acc)
      case _ => getAllNumbers(tail, List.empty, acc ::: List(currentNumber) )
    }
  }
}

def neighbours(enginePartNumber: List[EnginePart] ): List[EnginePart] = {
  val minX = enginePartNumber.minBy(_.xPos).xPos - 1
  val maxX = enginePartNumber.maxBy(_.xPos).xPos + 1
  val yPos = enginePartNumber.head.yPos

  def isXneighbour(enginePart: EnginePart): Boolean =
    enginePart.yPos == yPos && (enginePart.xPos == minX || enginePart.xPos == maxX)

  def isYNeighbour(enginePart: EnginePart, yPosToCheck: Int): Boolean =
    enginePart.yPos == yPosToCheck && (enginePart.xPos >= minX && enginePart.xPos <= maxX)

  allEngineParts.filter { part => part.yPos == yPos || part.yPos == yPos - 1 || part.yPos == yPos + 1 }
    .filter { part => isXneighbour(part) || isYNeighbour(part, yPos - 1) || isYNeighbour(part, yPos + 1) }
}

def isValidNumber(enginePartNumber: List[EnginePart] ): Boolean = {
  neighbours(enginePartNumber).find(part => part.char != '.').isDefined
}

/*
def isNeighbouring(asterist: EnginePart, engineParts: List[List[EnginePart]]): List[(EnginePart, List[EnginePart])] = {
  engineParts.flatMap { parts =>
    neighbours(parts).contains(asterist).option(_ => Some(asterist, engineParts))
  }
}
*/
//val hasNumberAdjacent(asterisk: EnginePart)

val allNumbers = getAllNumbers(allEngineParts)
val totalParts = allNumbers

  .filter(isValidNumber(_))
  .flatMap{
     enginePart => Try {
         enginePart.foldLeft(new StringBuilder()){ case (sb, part) => sb.append(part.char) }.toString.toInt
       }.toOption
     }
  .foldLeft(0){ case(total, part) => total + part }

def neighborForRatio(gearRatio: EnginePart, parts: List[EnginePart]): Option[(EnginePart, List[EnginePart])] =
  neighbours(parts).contains(gearRatio).option((gearRatio, parts))


val gearRatios:  List[List[(EnginePart, List[EnginePart])]] = allEngineParts.filter{part => part.char == '*'}
 .flatMap { gearRatio =>
   allNumbers.flatMap { parts => neighborForRatio(gearRatio, parts) }
 }







