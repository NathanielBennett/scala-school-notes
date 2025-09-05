package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, Cardinal, Grid, GridEntry}

import scala.util.Try

case class IntEntry(override val xPosition: Int, override val yPosition: Int, override val value: Int) extends GridEntry[Int]
case class IntGrid(override val entries: Set[GridEntry[Int]]) extends Grid[Int] {
  def update( updateEntries: Set[GridEntry[Char]] ): CharGrid = CharGrid( updateEntries )
}

trait DexemberTenParser extends AdventOfCodeGridParser[Int, IntGrid] {
  override def entryParser(x: Int, y: Int, char: Char): Option[GridEntry[Int]] =
    Try { Integer.parseInt(char.toString)}.toOption
      .map{ i => IntEntry(x, y, i) }

  override def gridMaker(entries: Set[GridEntry[Int]]): IntGrid = IntGrid(entries)
}

trait DecemberTen extends December[Int, IntGrid, Int] with DexemberTenParser {

  override def day = 10

  //TODO refactor listsizeSolver
  override def solver: Solver[Int, Int] = listTotalSolver(0, test)

  val grid = rawInput
  val trailHeads = grid.filterEntries( entry => entry.value == 0)


  def trailsForTrailHead(gridEntry: GridEntry[Int]): List[List[GridEntry[Int]]] = {

      def nextSteps(currentTrails: List[List[GridEntry[Int]]], nextStepCnt: Int): List[List[GridEntry[Int]]] = {
        if (nextStepCnt == 10) currentTrails
        else {
          val nextTrails = currentTrails.foldLeft(List[List[GridEntry[Int]]]()) {
            case (currentTrailsAcc, trail) => trail match {
              case Nil => currentTrailsAcc
              case head :: _ =>
                val nextStepEntries = grid.getFilteredNeighbours(head, cardinals) { gridEntry => gridEntry.value == nextStepCnt }
                if (nextStepEntries.isEmpty) currentTrailsAcc
                else nextStepEntries.map { nextStep => nextStep :: trail } ::: currentTrailsAcc
            }
          }
          nextSteps(nextTrails, nextStepCnt + 1)
        }
      }
      nextSteps(List(List(gridEntry)), 1)
  }
}

trait DecemberTenPartOne extends DecemberTen {

  def endsForTrail(trailHead: GridEntry[Int]): Int = {
    trailsForTrailHead(trailHead)
      .flatMap{ trail => trail.headOption }.toSet
      .size
  }

  override def rawSolution: List[Int] = trailHeads.map { th => endsForTrail(th) }
}

object DecemberTenPartOneTest extends DecemberTenPartOne with PuzzleTest
object DecemberTenPartOneSolution extends DecemberTenPartOne with PuzzleSolution

trait DecemberTenPartTwo extends DecemberTen {

  def endsForTrail(trail: GridEntry[Int]): Int = {
    trailsForTrailHead(trail).size
  }

  override def rawSolution: List[Int] = trailHeads.map { th => endsForTrail(th) }
}

object DecemberTenPartTwoTest extends DecemberTenPartTwo with PuzzleTest
object DecemberTenPartTwoSolution extends DecemberTenPartTwo with PuzzleSolution


