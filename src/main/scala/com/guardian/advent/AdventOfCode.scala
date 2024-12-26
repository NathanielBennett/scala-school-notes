package com.guardian.advent

import scala.collection.AbstractSeq
import scala.io.Source

trait InputFileReader {

  def test: Boolean
  def day: Int

  lazy val resourceName = test match {
    case true => s"day_${day}_test.txt"
    case false => s"day_$day.txt"
  }

  lazy val fileName = s"advent/2024/$resourceName"

  lazy val iterator = Source.fromResource(fileName).getLines()

  protected def getLines(filter: List[String] => List[String] = s => s) = filter(iterator.toList)
}

trait AdventOfCodeParser[T, U <: AbstractSeq[T]] extends InputFileReader with SolutionHelpers {

  def inputParser: String => Option[T]
  def sequenceToCollection(seq: Seq[T]) : U

  protected def parseLinesFromResource(toSeq: Iterator[String] => AbstractSeq[String]): U = {
    val sequence = toSeq(iterator).flatMap(line => inputParser(line)).toSeq
    sequenceToCollection(sequence)
  }
}

trait AdventOfCodeInput[T, U <: AbstractSeq[T], V, W <: AbstractSeq[W]] extends InputFileReader {

  def inputParser: AdventOfCodeParser[T,U]
  def instructionPatser: AdventOfCodeParser[V,W]

  def parser()(inputParser: String => Option[T], instructionParser: String => Option[U] ): (V, W) = {
    val rawInput = getLines( list => list.takeWhile { s => !s.isEmpty } )
    val rawInstruction = getLines( list => list.reverse.takeWhile { s => !s.isEmpty } )
    val input = in

    ( rawInput.flatMap(inputParser),  rawIn flatMap{ s => instructionParser(s) } )
  }
}

trait AdventOfCdeGrid[T, U <: AbstractSeq[T], U <: AbstractSeq[T], ANSWER] extends AdventOfCodeParser[T, List[T], ANSWER] {
  def gridParser(test: Boolean)(entryParser: (Int, Int, Char)  => Option[GridEntry[T]], gridMaker: Set[GridEntry[T]] => Grid[T] ) : Grid[T] = {
    val s = parseLinesFromResource(test)(s =>  s Some(s))
      .zipWithIndex
      .flatMap{
        case(rawEntries, yIndex) => ra
          rawEntries.toCharArray.toList.zipWithIndex.flatMap {
            case (char, xIndex) => entryParser(xIndex, yIndex, char)
          }
      }
    gridMaker(s.toSet)
  }

}



