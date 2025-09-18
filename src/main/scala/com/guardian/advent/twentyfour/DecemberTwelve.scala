package com.guardian.advent.twentyfour

import com.guardian.advent._

trait DecemberTwelveParser extends AdventOfCodeGridParser[Char, CharGrid] {
  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = Some(CharEntry(x, y, value))
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberTwelve extends December[Int, CharGrid, Int] with DecemberTwelveParser {

  override def day: Int = 12
  override def solver: Solver[Int, Int] = listTotalSolver(0, test)
  protected val grid = rawInput
  final val entrySides: Int = 4

  def findPlot(seedList: List[GridEntry[Char]], acc: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {
    seedList match {
      case Nil => acc
      case head :: tail =>
        val nextEntries = grid.getFilteredNeighbours(head, cardinals) { entry => !(seedList ::: acc).contains(entry) && entry.value == head.value }
        findPlot((tail ::: nextEntries), head :: acc)
    }
  }

  protected def findPlots(sortedEntries: List[GridEntry[Char]], plotsAcc: List[Set[GridEntry[Char]]] = List.empty ): List[Set[GridEntry[Char]]] = {
    sortedEntries match {
      case Nil => plotsAcc
      case head :: tail =>
        val plot = findPlot(List(head))
        findPlots(tail.diff(plot), plot.toSet :: plotsAcc)
    }
  }

  def edgePrice(plot: Set[GridEntry[Char]]): Int

  override def rawSolution: List[Int] = {
    findPlots(grid.sortedEntries).map {
      plot =>
        val perimeter = edgePrice(plot)
        perimeter * plot.size
    }
  }
}

trait DecemberTwelvePartOne extends DecemberTwelve {

  override def edgePrice(plot: Set[GridEntry[Char]]): Int = {
    plot.foldLeft(0) {
      case(perimeterTotal, plotEntry) =>
        val sides = grid.getFilteredNeighbours(plotEntry, cardinals){ entry => entry.value == plotEntry.value }
        perimeterTotal + (entrySides - sides.length )
    }
  }
}

object DecemberTwelvePartOneTest extends DecemberTwelvePartOne with PuzzleTest
object DecemberTwelvePartOneSolution extends DecemberTwelvePartOne with PuzzleSolution

trait DecemberTwelvePartTwo extends DecemberTwelve  {

  implicit class RichSet[A](set: Set[A]) {
    def apply(maybeA: Option[A]): Boolean = {
      maybeA.map { a => set(a) }.getOrElse(false)
    }
  }

  val quadrantDirections = List(
    (North, West,  NorthWest),
    (East, North, NorthEast),
    (South, East, SouthEast),
    (West, South, SouthWest)
  )

  override def edgePrice(plot: Set[GridEntry[Char]]): Int = {

    def entryForDirection(entriesAndDirections: List[(GridEntry[Char], Direction)], targetDirection: Direction): Option[GridEntry[Char]] =
      entriesAndDirections.find { case (_, direction) => targetDirection == direction }
        .map { case (gridEntry, _) => gridEntry }

    (for {
      entry <- plot.toList
      directedNeighbours = grid.getNeigboursAndDirections(entry)
      (rightCardinal, leftCardinal, semiCardinal) <- quadrantDirections
      right = entryForDirection(directedNeighbours, rightCardinal)
      left = entryForDirection(directedNeighbours, leftCardinal)
      semi = entryForDirection(directedNeighbours, semiCardinal)
      if (!plot(right) && !plot(left) || plot(right) && plot(left) && !plot(semi))
    } yield entry).size
  }
}

object DecemberTwelvePartTwoTest extends DecemberTwelvePartTwo with PuzzleTest
object DecemberTwelvePartTwoSolution extends DecemberTwelvePartTwo with PuzzleSolution
