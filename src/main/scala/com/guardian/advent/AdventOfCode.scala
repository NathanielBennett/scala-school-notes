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

  protected def getLines(filter: Option[List[String] => List[String]] = None): List[String] = {
    val lines = Source.fromResource(fileName).getLines().toList
    filter.map { f => f(lines) }.getOrElse(lines)
  }
}

trait SequenceConverter[T, U <: AbstractSeq[T]] {
  def toSeq(list: List[String] ): AbstractSeq[String]
}

trait AdventOfCodeParser[T, U <: AbstractSeq[T]] extends SequenceConverter[T,U]  with SolutionHelpers {

  def lineParser(line: String): Option[T]
  def sequenceToCollection(seq: Seq[T]) : U

  def parseLinesFromResource(lines: List[String]): U = {
    val sequence = toSeq(lines).flatMap(line => lineParser(line)).toSeq
    sequenceToCollection(sequence)
  }
}

trait AdventOfCodeInput[T, U <: AbstractSeq[T], V, W <: AbstractSeq[W]] extends InputFileReader {

  def inputParser: AdventOfCodeParser[T,U]
  def instructionPatser: AdventOfCodeParser[V,W]
  private def lineFilter(list: List[String]): List[String] = list.takeWhile{ s => !s.isEmpty}

  def parser(): (U, W) = {
    val rawInput = getLines{ Some( list  => lineFilter(list) ) }
    val rawInstruction  = getLines{ Some( list  => lineFilter(list.reverse) ) }

    ( inputParser.parseLinesFromResource(rawInput), instructionPatser.parseLinesFromResource(rawInstruction) )
  }
}

trait AdventOfCdeGrid[T] extends AdventOfCodeParser[String, List[String]] {

  def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[T]]
  def gridMaker(entries: Set[GridEntry[T]]): Grid[T]

  def gridParser(lines: List[String]) : Grid[T] = {
    val s = parseLinesFromResource(lines)
      .zipWithIndex
      .flatMap{
        case(rawEntries, yIndex) =>
          rawEntries.toCharArray.toList.zipWithIndex.flatMap {
            case (char, xIndex) => entryParser(xIndex, yIndex, char)
          }
      }
    gridMaker(s.toSet)
  }
}



