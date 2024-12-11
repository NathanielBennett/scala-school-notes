import scala.io.Source


//System.getenv("user.home")
val input = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day3test.TXT").getLines().toList
case class EnginePartNumber(char: Char, yPos: Int, xPos: Int)

def getAllNumbers(engineParts: List[EnginePartNumber],
                  currentNumber: List[EnginePartNumber] = List.empty,
                  acc: List[List[EnginePartNumber]] = List.empty): List[List[EnginePartNumber]] = engineParts match {
  case Nil => acc
  case head :: tail => head.char.isDigit match {
    case true => getAllNumbers(tail, currentNumber ::: List(head), acc)
    case false => currentNumber match {
      case List() => getAllNumbers(tail, currentNumber, acc)
      case _ => getAllNumbers(tail, List.empty, acc ::: List(currentNumber))

    }
  }
}

val allEngineParts = input.zipWithIndex.flatMap {
  case (line, yPos) => line.zipWithIndex.map { case (char, xPos) => EnginePartNumber(char, yPos, xPos) }
}

val allNumbers = getAllNumbers(allEngineParts)


