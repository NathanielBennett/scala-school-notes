package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCdeGridParser, Direction, Directions, Grid, GridEntry, InputFileReader}


case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char]

trait DecemberFourParser extends AdventOfCdeGridParser[Char, CharGrid] {

  override def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))

  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberFour extends DecemberFourParser with InputFileReader {

  final val xmas: String = "XMAS"
  override def day = 4

  def isXmas(entryList: List[GridEntry[Char]]): Boolean = entryList.map(_.value).mkString("") == xmas

  val lines = getLines()
  val grid = parseGrid(lines)
}

trait DecemberFourPartOne extends DecemberFour {
  override def solve(): Int =  grid.filterEntries{ ge => ge.value == 'X'}
    .flatMap { startPosition =>
      allDirections.map { direction => grid.vertice(startPosition, direction){ vertice => vertice.length == xmas.length } }
    }
    .filter {
      word => isXmas(word)
    }.size
}

object DecemberFourPartOneTest extends DecemberFourPartOne {
  override def test = true
}

object DecemberFourPartOneSolution extends DecemberFourPartOne {
  override def test = false
}

trait DecemberFourPartTwo extends DecemberFour {

  val crossBar = Set('M', 'S')

  def isCross(entriesAndDirections: Set[(GridEntry[Char], Direction)]): Boolean = {
     entriesAndDirections.size == 4 && {
         val (x, y) = entriesAndDirections.partition { case (_, direction) => oneDiagonal.contains(direction) }
         x.map{ case(entry, _) => entry.value} == crossBar && y.map { case(entry, _) => entry.value } == crossBar
     }
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

