package com.guardian.advent.twentyfive

import com.guardian.advent.{AdventOfCodeGridParser, DecemberTwentyFive}
import com.guardian.advent.grid.{CharGrid, GridEntry, Space, Block => Roll}
import com.guardian.advent.twentyfour.{PuzzleSolution, PuzzleTest, Solver}

import scala.util.Try

trait DecemberFourParser extends AdventOfCodeGridParser[Char, CharGrid] {

  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = Try {
    value match {
      case '.' => Space(x, y, value)
      case '@' => Roll(x, y, value)
    }
  }.toOption

  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberFour extends DecemberTwentyFive[Int, CharGrid, Int] with DecemberFourParser {

  override def day: Int = 4

  override def solver: Solver[Int, Int] = listSizeSolver

  val grid = rawInput

  protected def getNeighbours(roll: GridEntry[Char], grid: CharGrid): Int =
    grid.getFilteredNeighbours(roll, allDirections) { entry => isRoll(entry) }.length

  protected def getNeighbourCounts(grid: CharGrid): List[(GridEntry[Char], Set[GridEntry[Char]])] = {
    getRolls(grid).map {
      roll =>
        val neighbouringRolls = grid.getFilteredNeighbours(roll, allDirections) { entry =>isRoll(entry) }.toSet
        (roll, neighbouringRolls)
    }
  }

  protected def isRoll(entry: GridEntry[Char]): Boolean = Option(entry).collectFirst{ case roll: Roll => roll }.isDefined

  protected def getRolls(grid: CharGrid): List[GridEntry[Char]] = grid.filterEntries( entry => isRoll(entry) )
}

trait DecemberFourPartOne extends DecemberFour {

  override def rawSolution: List[Int] = {
    getNeighbourCounts(grid)
      .filter { case (_, neighbours) => neighbours.size < 4 }
      .map { case(_, neighbours) => neighbours.size }
  }
}

object DecemberFourPartOneTest extends DecemberFourPartOne with PuzzleTest
object DecemberFourPartOneSolution extends DecemberFourPartOne with PuzzleSolution

trait DecemberFourPartTwo extends DecemberFour {

   override def solver: Solver[Int, Int] = listTotalSolver(0)

   override def rawSolution: List[Int] = {
      val rollsAndNeighbours = getNeighbourCounts(grid)
      renoveAllRolls(rollsAndNeighbours)
   }

   private def getMovableRolls(rollsAndNeighbours: List[(GridEntry[Char], Set[GridEntry[Char]])]): Set[GridEntry[Char]] = {
     rollsAndNeighbours.filter{ case (_, neighbours) => neighbours.size < 4 }
       .map{ case(roll, _) => roll }.toSet
   }

   private def renoveAllRolls(rollsAndNeighbours: List[(GridEntry[Char], Set[GridEntry[Char]])], acc: List[Int] = List.empty): List[Int] = {

      val movableRolls = getMovableRolls(rollsAndNeighbours)

      if(movableRolls.isEmpty) acc
      else {
        val updatedRollsAndNeighbours = rollsAndNeighbours
          .filterNot{ case(roll, _) => movableRolls.contains(roll) }
          .map {case (roll, neighbrours) => (roll, neighbrours.diff(movableRolls) )
          }
        renoveAllRolls(updatedRollsAndNeighbours, movableRolls.size :: acc )
      }
   }
}

object DecemberFourPartTwoTest extends DecemberFourPartTwo with PuzzleTest
object DecemberFourPartTwoSolution extends DecemberFourPartTwo with PuzzleSolution
