package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCode, Grid, GridEntry}

case class CharEntry[Char](override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid[Char](override val entries: Set[GridEntry[Char]]) extends Grid[Char]

trait DecemberFour extends AdventOfCode with App {
  override def day = 4


   val gred = gridParser[Char]()


}

object DecemberFourPartOneTest extends DecemberFour {
  println("Hello")

}
