package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCode, Direction, Directions, Grid, GridEntry, NorthWest, SouthEast}
import com.sun.xml.internal.messaging.saaj.util.CharWriter

case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char]

trait DecemberFour extends AdventOfCode with App with Directions {

  final val xmas: String = "XMAS"
  override def day = 4
  def test: Boolean = false
  
  private def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))

  private def gridMaker(entries: Set[GridEntry[Char]]): Grid[Char] = CharGrid(entries)

  def isXmas(entryList: List[GridEntry[Char]]): Boolean = entryList.map(_.value).mkString("") == xmas

  val grid: CharGrid = gridParser[Char](test) (entryParser, gridMaker).asInstanceOf[CharGrid]
  val startPositions = grid.filterEntries(entry => entry.value == 'X')
}

trait DecemberFourPartOne extends DecemberFour {
  val words = grid.filterEntries{ ge => ge.value == 'X'}
    .flatMap { startPosition =>
      allDirections.map { direction => grid.vertice(startPosition, direction){ vertice => vertice.length == xmas.length } }
    }
    .filter {
      word => isXmas(word)
    }
    println(words.length)
}

object DecemberFourPartOneTest extends DecemberFourPartOne {
  override def test = true
  //18
}

object DecemberFourPartOneProd extends DecemberFourPartOne //2434

 trait DecemberFourPartTwo extends DecemberFour {

  val crossBar = Set('M', 'S')

  def isCross(entriesAndDirections: Set[(GridEntry[Char], Direction)]): Boolean = {
     entriesAndDirections.size == 4 && {
         val (x, y) = entriesAndDirections.partition { case (_, direction) => oneDiagonal.contains(direction) }
         x.map{ case(entry, _) => entry.value} == crossBar && y.map { case(entry, _) => entry.value } == crossBar
     }
   }

   val crosses = grid.filterEntries{ gridEntry => gridEntry.value == 'A' }
    .map { startPosition => grid.getNeigboursAndDirections( startPosition, allDirections.filterNot(_.isCardinal) ).toSet }
    .filter { maybeCross => isCross(maybeCross) }

   println(crosses.length )
}

object DecemberFourPartTwoTest extends DecemberFourPartTwo {
  override def test = true
}

object DecemberFourPartTwoProd extends DecemberFourPartTwo {
  override def test: Boolean = false //1835
}

