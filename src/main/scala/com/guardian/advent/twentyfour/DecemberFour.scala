package com.guardian.advent.twentyfour

import com.guardian.advent.grid.{CharEntry, CharGrid, Direction, GridEntry}
import com.guardian.advent.twentyfour.DecemberFourPartOneSolution.Cross
import com.guardian.advent.AdventOfCodeGridParser

trait DecemberFourParser extends AdventOfCodeGridParser[Char, CharGrid] {
  override def entryParser(xPos: Int, yPos: Int, char: Char):  Option[GridEntry[Char]] = Some(CharEntry(xPos, yPos, char))
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberFour[T] extends December[Int, CharGrid, T] with DecemberFourParser {
  type Cross = Set[(GridEntry[Char], Direction)]

  override def day = 4
  override def solver: Solver[T, Int] = listSizeSolver
  val grid = rawInput
}

trait DecemberFourPartOne extends DecemberFour[String] {

  final val xmas: String = "XMAS"
  final lazy val xMasLength = xmas.length

  override def rawSolution: List[String] = {
    val t = grid.filterEntries( ge => ge.value == 'X')
    .flatMap{ startPosition =>
      val entries = allDirections.map { direction => grid.vertice(startPosition, direction) { case (entry, vertice) => (entry :: vertice).length > xMasLength }.reverse }
      entries.map{ ens => gridEntryListToString(ens) }
    }
    t.filter( w  => w == xmas )
  }
}

object DecemberFourPartOneTest extends DecemberFourPartOne with PuzzleTest
object DecemberFourPartOneSolution extends DecemberFourPartOne with PuzzleSolution

trait DecemberFourPartTwo extends DecemberFour[Cross] {

  val crossBar = Set('M', 'S')

  private def isCross(entriesAndDirections: Set[(GridEntry[Char], Direction)]): Boolean = {
    entriesAndDirections.size == 4 && {
      val (x, y) = entriesAndDirections.partition { case (_, direction) => oneDiagonal.contains(direction) }
      x.map { case (entry, _) => entry.value } == crossBar && y.map { case (entry, _) => entry.value } == crossBar
    }
  }

  override def rawSolution: List[Cross] = {
    grid.filterEntries { gridEntry => gridEntry.value == 'A' }
      .map { startEntry => grid.getNeigboursAndDirections(startEntry, nonCardinals).toSet }
      .filter { maybeCross => isCross(maybeCross) }
  }
}

object DecemberFourPartTwoTest extends DecemberFourPartTwo with PuzzleTest
object DecemberFourPartTwoSolution extends DecemberFourPartTwo with PuzzleSolution

