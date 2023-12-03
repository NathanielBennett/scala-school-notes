import scala.io.Source

val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day3test.txt").getLines().toList
case class EnginePart(char: Char, yPos: Int, xPos: Int) {

}

def getAllNumbers(engineParts: List[EnginePart],
                  currentNumber: List[EnginePart] = List.empty,
                  acc: List[List[EnginePart]] = List.empty): List[List[EnginePart]] = engineParts match {
  case Nil => acc
  case head :: tail => head.char.isDigit match {
    case true => getAllNumbers(tail, currentNumber ::: List(head), acc)
    case false => currentNumber match {
      case List() => getAllNumbers(tail, currentNumber, acc)
      case _ => getAllNumbers(tail, List.empty, acc ::: List(currentNumber))

    }
  }
}



class Engine(engineParts: List[EnginePart]) {

  def getNeighboursFor(enginePartNumber: List[EnginePart]): List[EnginePart] = {
      val xNeigbours = engineParts.filter {
        part => part.yPos == enginePartNumber.head.yPos &&
          part.xPos == enginePartNumber.maxBy(_.xPos).xPos + 1 &&
          part.xPos == enginePartNumber.minBy(_.xPos).xPos - 1
      }


    }
  }
}

val allEngineParts = input.zipWithIndex.flatMap {
  case (line, yPos) => line.zipWithIndex.map { case (char, xPos) => EnginePart(char, yPos, xPos) }
}

val allNumbers = getAllNumbers(allEngineParts)


