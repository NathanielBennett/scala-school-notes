package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, GridEntry}

sealed trait AntennaGridEntry extends GridEntry[Char]
case class EmptyAntenna (override val xPosition: Int, override val yPosition: Int)  extends AntennaGridEntry {
  override val value = '.'
}

case class Antinode (override val xPosition: Int, override val yPosition: Int)  extends AntennaGridEntry {
  override val value = '#'
}

case class Antenna (override val xPosition: Int, override val yPosition: Int, override val value: Char)  extends AntennaGridEntry

object AntennaGridEntry {
  def apply(xPosition: Int, yPosition: Int, char: Char): Option[AntennaGridEntry] = char match {
    case '.' => Some(EmptyAntenna(xPosition, yPosition))
    case c: Char => (Some(Antenna(xPosition, yPosition, c)))
    case _  => None
  }
}

trait DecemberEightParser extends AdventOfCodeGridParser[Char, CharGrid] {
  override def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = AntennaGridEntry(xPos, yPos, char)
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberEight extends December[Int, CharGrid, GridEntry[Char]] with DecemberEightParser {
  override def day: Int = 8

  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  protected val grid = rawInput

  override def rawSolution: List[GridEntry[Char]] = ???

}

 trait DecemberEightPartOne extends DecemberEight
 object DecemberEightPartOneText extends DecemberEightPartOne with PuzzleTest with App {
   grid.printGrid()
   val antennas = grid.matchingEntries{
     entries => entries.collect {
       case antenna: Antenna => antenna
     }
   }
   val gr = antennas.groupBy { ant => ant.value }
   gr.foreach {
     case (frequency, antennas) => println(s"$frequency:")
       val pairs = antennas.toList.combinations(2).toSet
       pairs.flatMap{ p => p.toTuple }
         .foreach{ case (a, b) =>
            println(s"($a, $b)")
            println(s"a: ${a.abs(b)}")
            println(s"b: ${b.abs(a)}")
         }
   }

 }
