package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeGridParser
import com.guardian.advent.grid.{CharGrid, GridEntry}

sealed trait AntennaGridEntry extends GridEntry[Char] {
  def antinodeCnt: Int
}

case class Antinode (override val xPosition: Int, override val yPosition: Int, override val antinodeCnt: Int = 0)  extends AntennaGridEntry {
  override val value = '.'
}

case class Antenna (override val xPosition: Int, override val yPosition: Int, override val value: Char, override val antinodeCnt: Int = 0)  extends AntennaGridEntry

object AntennaGridEntry {

  def apply(xPosition: Int, yPosition: Int, char: Char): Option[AntennaGridEntry] = char match {
    case '.' => Some(Antinode(xPosition, yPosition))
    case c: Char => (Some(Antenna(xPosition, yPosition, c)))
    case _  => None
  }
}

trait DecemberEightParser extends AdventOfCodeGridParser[Char, CharGrid] {

  override def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = AntennaGridEntry(xPos, yPos, char)
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberEight extends December[Int, CharGrid, AntennaGridEntry] with DecemberEightParser {

  override def day: Int = 8
  override def solver: Solver[AntennaGridEntry, Int] = listSizeSolver
  protected val grid = rawInput

  lazy val antennaPairs = grid.entries.collect{ case antenna: Antenna => antenna }
    .groupBy{ case antenna: Antenna => antenna.value}
    .flatMap { case(_, antennas) => antennas.toList.combinations(2) }
    .toList
}

 trait DecemberEightPartOne extends DecemberEight {

   override def rawSolution: List[AntennaGridEntry] = {1
     val t = antennaPairs
       .flatMap {
         case antennas =>
           val(a, b) = (antennas(0), antennas(1))
           val diff = b.minus(a)
           List(a.minus(diff), b.plus(diff))

       }
       .toSet
       .toList

     t.flatMap{ case (x, y) =>
       grid.findEntry(x, y).collect { case antennaGridEntry: AntennaGridEntry => antennaGridEntry }
     }
   }
 }

object DecemberEightPartOneTest extends DecemberEightPartOne with PuzzleTest
object DecemberEightPartOneSolution extends DecemberEightPartOne with PuzzleSolution

trait DecemberEightPartTwo extends DecemberEight {

  def makeList(acc: List[AntennaGridEntry])(maybeNextEntry: AntennaGridEntry => (Int, Int)): List[AntennaGridEntry] = {
    acc.headOption.flatMap { ant =>
      val (x, y) = maybeNextEntry(ant)
      grid.findEntry(x, y).collect { case antennaGridEntry: AntennaGridEntry => antennaGridEntry }
    } match {
      case Some(antinode) => makeList(antinode :: acc)(maybeNextEntry)
      case None => acc
    }
  }

  override def rawSolution: List[AntennaGridEntry] = {
    antennaPairs
      .flatMap { antennas =>
        val (antennaA, antennaB) = (antennas(0), antennas(1))
        val diff = antennaB.minus(antennaA)
        makeList(List(antennaA)) { ant => ant.minus(diff) } ++ makeList(List(antennaB)) { ant => ant.plus(diff) }
      }
      .toSet
      .toList
  }
}

object DecemberEightPartTwoTest extends DecemberEightPartTwo with PuzzleTest
object DecemberEightPartTwoSolution extends DecemberEightPartTwo with PuzzleSolution