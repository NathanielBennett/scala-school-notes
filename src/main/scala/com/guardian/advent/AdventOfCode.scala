package com.guardian.advent

import scala.io.Source



trait AdventOfCode[ANSWER] extends SolutionHelpers {

  def solve(): ANSWER

  val rootPath = s"${System.getProperty("user.home")}/advent2023/2024"
  def test: Boolean
  def day: Int

  protected def parseLinesFromResource[T](test: Boolean = false)(parser: String => Option[T] ): List[T] = {
    val fileName = makeFilename(test)
    Source.fromResource(s"advent/2024/$fileName").getLines().toList
      .flatMap { line => parser(line)}
  }

  private def makeFilename(test: Boolean): String = test match {
    case true => s"day_${day}_test.txt"
    case false => s"day_$day.txt"
  }

  def gridParser[T](test: Boolean)(entryParser: (Int, Int, Char)  => Option[GridEntry[T]], gridMaker: Set[GridEntry[T]] => Grid[T] ) : Grid[T] = {
   val s = parseLinesFromResource[String](test)(s => Some(s))
     .zipWithIndex
     .flatMap{
        case(rawEntries, yIndex) =>
          rawEntries.toCharArray.toList.zipWithIndex.flatMap {
            case (char, xIndex) => entryParser(xIndex, yIndex, char)
          }
     }
     gridMaker(s.toSet)
  }

  def instructionParser[S,T](test: Boolean)(inputParser: String => Option[S], instructionParser: String => Option[T] ): (List[S], List[T]) = {
    val allLines = parseLinesFromResource(test) { s => Some(s) }
    val rawInput = allLines.takeWhile { s => !s.isEmpty }
    val rawInstructions = allLines.reverse.takeWhile { s => !s.isEmpty }.reverse
    ( rawInput.flatMap{ s => inputParser(s) }, rawInstructions.flatMap{ s => instructionParser(s) } )
  }
}

trait AdventOfCodeRunner[T] extends AdventOfCode[T]


