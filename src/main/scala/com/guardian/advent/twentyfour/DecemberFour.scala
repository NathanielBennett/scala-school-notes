package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCode, Direction, Directions, Grid, GridEntry}


case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char]

trait DecemberFour[T] extends AdventOfCode[T] with App with Directions {

  final val xmas: String = "XMAS"
  override def day = 4

  private def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))

  private def gridMaker(entries: Set[GridEntry[Char]]): Grid[Char] = CharGrid(entries)

  def isXmas(entryList: List[GridEntry[Char]]): Boolean = entryList.map(_.value).mkString("") == xmas

  val grid: CharGrid = gridParser[Char](test) (entryParser, gridMaker).asInstanceOf[CharGrid]
}

trait DecemberFourPartOne extends DecemberFour[Int] {
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
  //18
}

object DecemberFourPartOneProd extends DecemberFourPartOne //2434

 trait DecemberFourPartTwo extends DecemberFour[Long] {

  val crossBar = Set('M', 'S')

  def isCross(entriesAndDirections: Set[(GridEntry[Char], Direction)]): Boolean = {
     entriesAndDirections.size == 4 && {
         val (x, y) = entriesAndDirections.partition { case (_, direction) => oneDiagonal.contains(direction) }
         x.map{ case(entry, _) => entry.value} == crossBar && y.map { case(entry, _) => entry.value } == crossBar
     }
   }

   def solve() : Long = {
     grid.filterEntries { gridEntry => gridEntry.value == 'A' }
       .map { startPosition => grid.getNeigboursAndDirections(startPosition, allDirections.filterNot(_.isCardinal)).toSet }
       .filter { maybeCross => isCross(maybeCross) }
       .size
   }
}

object DecemberFourPartTwoTest extends DecemberFourPartTwo {
  override def test = true
}

object DecemberFourPartTwoProd extends DecemberFourPartTwo {
  override def test: Boolean = false //1835
}

