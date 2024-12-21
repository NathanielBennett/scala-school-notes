package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCode, Grid, GridEntry}

case class CharEntry[Char](override val xPosition: scala.Int, override val yPosition: scala.Int, override val value: Char) extends GridEntry[Char]
case class CharGrid[CharEntry](override val entries: Set[CharEntry]) extends Grid[CharEntry]

trait DecemberFour extends AdventOfCode with App {
  override def day = 4
  
  def parseRow(row: String): Option[List[Char]] = Some(row.toCharArray.toList)
  def makeGrid(entries: Set[Char]) = CharGrid(entries)
  val grid = gridParser[List[Char], CharEntry](true)(parseRow, makeGrid)

}

object DecemberFourPartOne extends DecemberFour {
  println("Hello")

}
