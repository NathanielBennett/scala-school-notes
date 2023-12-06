package com.guardian.advent

import scala.io.Source
import scala.util.Try



object TestObject extends App {

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

  def getNeighbours(enginePartNumber: List[EnginePart] ): List[EnginePart] = {
    val minX = enginePartNumber.minBy(_.xPos).xPos - 1
    val maxX = enginePartNumber.maxBy(_.xPos).xPos + 1
    val yPos = enginePartNumber.head.yPos
    def isXneighbour(enginePart: EnginePart): Boolean =
      enginePart.yPos == yPos && (enginePart.xPos == minX || enginePart.xPos == maxX)
    def isYNeighbour(enginePart: EnginePart, yPosToCheck: Int): Boolean =
      enginePart.yPos == yPosToCheck && (enginePart.xPos >= minX && enginePart.xPos <= maxX)

    //allEngineParts.filter{ part => part.yPos == yPos || part.yPos == yPos -1 || part.yPos == yPos + 1}
    allEngineParts.filter{ part =>

      isXneighbour(part) ||
        isYNeighbour(part, yPos - 1) || isYNeighbour(part, yPos + 1)  }

  }

  val allNumbers = getAllNumbers(allEngineParts)
  val testa = getNeighbours(allNumbers(0))


}
