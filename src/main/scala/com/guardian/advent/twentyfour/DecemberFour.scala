package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCode, Direction, Grid, GridEntry}

case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char] {
  def getAllVerticesForEntry(entry: GridEntry[Char]): List[List[GridEntry[Char]) = {
     def nextVertice(directions: List[Direction]; acc: List[List]
  }
}

trait DecemberFour extends AdventOfCode with App {
  override def day = 4
  def test: Boolean

  private def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))
  private def gridMaker(entries: Set[GridEntry[Char]]): Grid[Char] = CharGrid(entries)

  val grid: Grid[Char] = gridParser[Char](test) (entryParser, gridMaker)
  val startPositions = grid.filterEntries(entry => entry.value == 'X')
}

object DecemberFourPartOneTest extends DecemberFour {
  override def test = true
  grid.printGrid()
  startPositions.flatMap
}
