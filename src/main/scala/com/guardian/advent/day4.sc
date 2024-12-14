import scala.io.Source
import scala.util.Try

<<<<<<< HEAD
=======
implicit class RichBoolean(val b: Boolean) extends AnyVal {
  final def option[A](a: => A): Option[A] = if (b) Some(a) else None
}

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day3.txt").getLines().toList
case class EnginePart(char: Char, yPos: Int, xPos: Int) {
>>>>>>> origin/main

//System.getenv("user.home")
val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day3test.TXT").getLines().toList
case class EnginePartNumber(char: Char, yPos: Int, xPos: Int)

<<<<<<< HEAD
def getAllNumbers(engineParts: List[EnginePartNumber],
                  currentNumber: List[EnginePartNumber] = List.empty,
                  acc: List[List[EnginePartNumber]] = List.empty): List[List[EnginePartNumber]] = engineParts match {
=======

val allEngineParts = input.zipWithIndex.flatMap {
  case (line, yPos) => line.zipWithIndex.map { case (char, xPos) => EnginePart(char, yPos, xPos) }
}

def getAllNumbers(engineParts: List[EnginePart],
                  currentNumber: List[EnginePart] = List.empty,
                  acc: List[List[EnginePart]] = List.empty): List[List[EnginePart]] = engineParts match {
>>>>>>> origin/main
  case Nil => acc
  case head :: tail => head.char.isDigit match {
    case true => getAllNumbers(tail, currentNumber ::: List(head), acc)
    case false => currentNumber match {
      case List() => getAllNumbers(tail, currentNumber, acc)
      case _ => getAllNumbers(tail, List.empty, acc ::: List(currentNumber) )
    }
  }
}

<<<<<<< HEAD
val allEngineParts = input.zipWithIndex.flatMap {
  case (line, yPos) => line.zipWithIndex.map { case (char, xPos) => EnginePartNumber(char, yPos, xPos) }
=======
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
>>>>>>> origin/main
}


def enginePartsToMayBeInt(engineParts: List[EnginePart]): Option[Int] = Try {
  engineParts.foldLeft(new StringBuilder()){ case (sb, part) => sb.append(part.char) }.toString().toInt
}.toOption

val allNumbers = getAllNumbers(allEngineParts)
val totalParts = allNumbers

  .filter(isValidNumber(_))
  .flatMap{ enginePart => enginePartsToMayBeInt(enginePart) }
  .foldLeft(0){ case(total, part) => total + part }

def neighborForRatio(gearRatio: EnginePart, parts: List[EnginePart]): Option[(EnginePart, List[EnginePart])] =
  neighbours(parts).contains(gearRatio).option((gearRatio, parts))

val gearRatios2 = allEngineParts.filter{part => part.char == '*'}
 .map { gearRatio =>
   allNumbers.flatMap { parts => neighborForRatio(gearRatio, parts) }
 }
  .filter( partList => partList.length == 2)
  .map { gearRatio => gearRatio.map{ case(_, parts) => parts } }
  .map {  gearRatios => (gearRatios.head, gearRatios.reverse.head) }
  .flatMap{ case(gearOne, gearTwo) => for {
      totalOne <- enginePartsToMayBeInt(gearOne)
      totalTwo <- enginePartsToMayBeInt(gearTwo)
    } yield totalOne * totalTwo
  }
  .foldLeft(0){case(total, gear) => total + gear}



