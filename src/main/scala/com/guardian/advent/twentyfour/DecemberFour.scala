package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, Direction, Grid, GridEntry, InputFileReader}


case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char]

trait DecemberFourParser extends AdventOfCodeGridParser[Char, CharGrid] {

  override def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberFour extends December[Int, String] with DecemberFourParser with InputFileReader with ListSizeSolver[String] {

  val grid = rawInput

  override def day = 4
}

trait DecemberFourPartOne extends DecemberFour[String] {

  final val xmas: String = "XMAS"
  final lazy val xMasLength = xmas.length

  override def rawSolution: List[String] =
    grid.filterEntries( ge => ge.value == 'X')
    .flatMap{ startPosition =>
      val entries = allDirections.map { direction => grid.vertice(startPosition, direction) { vertice => vertice.length == xMasLength } }
      entries.map{ ens => gridEntryListToString(ens)}
    }
    .filter( w  => w == xmas )
}

object DecemberFourPartOneTest extends DecemberFourPartOne with PuzzleTest
object DecemberFourPartOneSolution extends DecemberFourPartOne with PuzzleSolution

trait DecemberFourPartTwo extends DecemberFour {

  val crossBar = Set('M', 'S')

  def isCross(entriesAndDirections: Set[(GridEntry[Char], Direction)]): Boolean = {
     entriesAndDirections.size == 4 && {
         val (x, y) = entriesAndDirections.partition { case (_, direction) => oneDiagonal.contains(direction) }
         x.map{ case(entry, _) => entry.value} == crossBar && y.map { case(entry, _) => entry.value } == crossBar
     }
   }

  override def rawSolution: List[String] = {
    val t = grid.filterEntries { gridEntry => gridEntry.value == 'A' }
      .map { startEntry => grid.getNeigboursAndDirections(startEntry, nonCardinals).toSet }
      .filter { maybeCross => isCross(maybeCross) }
      .


  }
  def solve() : Int = {
     grid.filterEntries { gridEntry => gridEntry.value == 'A' }
       .map { startPosition => grid.getNeigboursAndDirections(startPosition, allDirections.filterNot(_.isCardinal)).toSet }
       .filter { maybeCross => isCross(maybeCross) }
       .size
   }
}

object DecemberFourPartTwoTest extends DecemberFourPartTwo {
  override def test = true
}

object DecemberFourPartTwoSolution extends DecemberFourPartTwo {
  override def test: Boolean = false //1835
}

