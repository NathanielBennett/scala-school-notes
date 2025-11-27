package com.guardian.advent

import com.guardian.advent.grid.{Directions, Grid, GridEntry}
import com.guardian.advent.parsers.StringParser
import com.guardian.advent.twentyfour.RawInputProvider

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

trait AdventOfCodeParser[T, U <: AbstractSeq[T]] extends SequenceConverter[T,U]  with RawInputProvider[U] {

  def lineParser(line: String): Option[T]
  def sequenceToCollection(seq: Seq[T]) : U

  override def toSeq(list: List[String]): AbstractSeq[String] = list

  override def rawInput: U = {
    val lines = getLines()
    parseLinesFromResource(lines)
  }

  def parseLinesFromResource(lines: List[String]): U = {
    val sequence = toSeq(lines).flatMap(line => lineParser(line)).toSeq
    sequenceToCollection(sequence)
  }
}

trait MultiLineParser[T, U <: AbstractSeq[T]] extends AdventOfCodeParser[T, U] {
  override def toSeq(list: List[String] ): AbstractSeq[String] = list
  override def lineParser(line: String): Option[T] = None

  protected def groupRawLines(lines: List[String], acc: List[List[String]] = List.empty, currRoughObject: List[String] = List.empty): List[List[String]] = {
    lines match {
      case Nil =>
        (currRoughObject :: acc).reverse
      case head :: tail =>
        if (head.isEmpty) groupRawLines(tail, currRoughObject :: acc, List.empty)
        else groupRawLines(tail, acc, head :: currRoughObject)
    }
  }
}



trait AdventOfCodeInstructionsParser[T, U <: AbstractSeq[T], V, W <: AbstractSeq[V]] extends RawInputProvider[(U, W)] {

  def inputParser: AdventOfCodeParser[T,U]
  def instructionParser: AdventOfCodeParser[V,W]

  private def lineFilter(list: List[String]): List[String] = list.takeWhile{ s => !s.isEmpty }

  override def rawInput: (U, W) =  {
    val rawInput = getLines{ Some( list  => lineFilter(list) ) }

    //TODO sort
    val rawInstruction  = getLines{ Some( list  => lineFilter(list.reverse) ) }.reverse

    ( inputParser.parseLinesFromResource(rawInput), instructionParser.parseLinesFromResource(rawInstruction) )
  }
}

trait GridComboParser[T, GRID <: Grid[T], A, B <: AbstractSeq[A]] extends RawInputProvider[(GRID, B)] {

  def gridParser: AdventOfCodeGridParser[T, GRID]
  def instructionParser: AdventOfCodeParser[A, B]

  private def lineFilter(list: List[String]): List[String] = list.takeWhile{ s => !s.isEmpty }

  override def rawInput: (GRID, B) = {
    val rawGrid = getLines {Some(list => lineFilter(list))}
    val rawInstructions = getLines( Some( list => lineFilter(list.reverse) ) ).reverse

    val grid = gridParser.parseGrid(rawGrid)
    val instructions = instructionParser.parseLinesFromResource(rawInstructions)

    (grid, instructions)
  }
}

trait AdventOfCodeGridParser[T, GRID <: Grid[T]] extends RawInputProvider[GRID]  with Directions {

  val stringParser = new StringParser {
    override def test: Boolean = this.test
    override def day: Int = this.day
  }

  def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[T]]
  def gridMaker(entries: Set[GridEntry[T]]): GRID

  override def rawInput: GRID = {
    val lines = getLines()
    parseGrid(lines)
  }

  def parseGrid(lines: List[String]) : GRID = {
    val s = stringParser.parseLinesFromResource(lines)
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

trait AdventOfCodeObjectParser[T] extends RawInputProvider[T] {

  def linesToObject(lines: List[String]): T

  override def rawInput: T = {
    val lines = getLines()
    linesToObject(lines)
  }
}



