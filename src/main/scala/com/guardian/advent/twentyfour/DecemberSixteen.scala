package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeGridParser
import com.guardian.advent.grid.{Block, Cardinal, CharEntry, CharGrid, East, End, GridEntry, South, Space, Start}

import scala.util.Try

trait DecembemberSixteenParser extends AdventOfCodeGridParser[Char, CharGrid] {

  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = Try {
    value match {
      case '.' => Space(x, y, value)
      case '#' => Block(x, y, value)
      case 'S' => Start(x, y, value, East)
      case 'E' => End(x, y, value)
    }
  }.toOption

  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberSixteen[T] extends December[Int, CharGrid, T] with DecembemberSixteenParser {

  private type PathEntry = (GridEntry[Char], Cardinal, Int)

  override def day = 16

  val grid = rawInput
  val maybeStart = grid.findEntry('S')

  protected implicit def pathEntryToEntry(pathEntry: PathEntry): GridEntry[Char] = pathEntry._1

  private def isEnd(pathEntry: PathEntry): Boolean = {
    Option(pathEntry._1).collectFirst { case end: End => end }.isDefined
  }

  private def getNextStepsForPath(currentStep: PathEntry, seenEntries: Map[(GridEntry[Char], Cardinal), PathEntry] = Map.empty, grid: CharGrid): List[PathEntry] = {

    def lowerThanPreviousPath(pathEntry: PathEntry): Boolean = {
      val (entry, cardinal, points) = pathEntry
      seenEntries.get((entry, cardinal)) match {
        case Some((_, _, seenPoints)) => points <= seenPoints
        case _ => true
      }
    }

    val (entry, cardinal, currentPoints) = currentStep //implicit

    (for {
      (nextCardinal, points) <- List((cardinal, currentPoints + 1), (cardinal.nextCardinal, currentPoints + 1001), (cardinal.previousCardinal, currentPoints + 1001))
      nextEntry <- grid.nextEntryByDirection(entry, nextCardinal)
      space <- Option(nextEntry).collectFirst {
        case start: Start => start
        case space: Space => space
        case end: End => end
      }
      if lowerThanPreviousPath((space, nextCardinal, points))
    } yield (space, nextCardinal, points))
  }

  private def updateSeenSteps(nextSteps: List[PathEntry], seen: Map[(GridEntry[Char], Cardinal), PathEntry]): Map[(GridEntry[Char], Cardinal), PathEntry] = {
    val next = (for {
      (entry, cardinal, points) <- nextSteps
      _ <- Option(entry).collectFirst {
        case start: Start => start
        case space: Space => space
      }
    } yield (entry, cardinal) -> (entry, cardinal, points)).toMap
    seen ++ next
  }

  protected def pathsThroughMaze(start: PathEntry, grid: CharGrid): List[List[PathEntry]] = {
    def nextSteps(current: List[List[PathEntry]], seen: Map[(GridEntry[Char], Cardinal), PathEntry], completedPaths: List[List[PathEntry]] = List.empty, cnt: Int = 0): (List[List[PathEntry]], List[List[PathEntry]]) = {

     current match {
        case Nil => (current, completedPaths)
        case _ =>
          val toFold = (List[List[PathEntry]](), seen, completedPaths)
          val (updatedPaths, nowSeen, nowCompleted) = current.foldLeft(toFold) {
            case (folded, currentPath) =>
              val (currentPaths, currentSeen, currentCompleted) = folded

              currentPath match {
                case Nil =>
                  (currentPaths, currentSeen, currentCompleted)
                case head :: _ =>
                  getNextStepsForPath(head, currentSeen, grid) match {
                    case Nil => (currentPaths, currentSeen, currentCompleted)
                    case nextSteps =>
                      val (nextPaths, nextCompletedPaths) = nextSteps.find { pathEntry => isEnd(pathEntry) }
                        .map { end =>
                          (currentPaths.filterNot { path => path == currentPath }, (end :: currentPath) :: currentCompleted)
                        }
                        .getOrElse {
                          val t = nextSteps.map { step => step :: currentPath } ::: currentPaths
                          (t, currentCompleted)
                        }
                      val nextSeen = updateSeenSteps(nextSteps, currentSeen)
                      (nextPaths, nextSeen, nextCompletedPaths)
                  }
              }
          }
          nextSteps(updatedPaths, nowSeen, nowCompleted, cnt + 1)
      }
    }

    val startPaths = List(List(start))
    val (entry, cardinal, _) = start
    val startSeen = Map((entry, cardinal) -> start)
    val (current, completedPath) = nextSteps(startPaths, startSeen)
    current ++ completedPath
  }
}

trait DecemberSixteenPartOne extends DecemberSixteen[Int] {

  override def solver: Solver[Int, Int] = new Solver[Int, Int] {
    override def solution(list: List[Int]): Int = list.min
  }

  override def rawSolution: List[Int] = {

    maybeStart.map {
       rawStart =>
         val decoratedPaths = pathsThroughMaze((rawStart, East, 0), grid)
         decoratedPaths.flatMap {
           path => path.headOption.map{ case(_, _, score) => score}
         }
    }.getOrElse(List.empty)
  }
}

trait DecemberSixteenPartTwo extends DecemberSixteen[GridEntry[Char]] {

  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  override def rawSolution: List[GridEntry[Char]] = {

    maybeStart.map {
      rawStart =>
        val decoratedPaths = pathsThroughMaze((rawStart, East, 0), grid)
        val bestPathScore = decoratedPaths.flatMap {
          path => path.headOption.map{ case(_, _, score) => score}
        }.min

        val bestPaths = decoratedPaths.filter {
          path =>
            val pathScore = path.headOption.map{ case(_, _, score) => score }.getOrElse(0)
            pathScore == bestPathScore
        }

         (for {
          path <- bestPaths
          (entry, _, _) <- path
        } yield entry ).distinct
    }.getOrElse(List.empty)
  }
}

object DecemberSixteenPartOneTest extends DecemberSixteenPartOne with PuzzleTest
object DecemberSixteenPartOneSolution extends DecemberSixteenPartOne with PuzzleSolution

object DecemberSixteenPartTwoTest extends DecemberSixteenPartTwo with PuzzleTest
object DecemberSixteenPartTwoSolution extends DecemberSixteenPartTwo with PuzzleSolution
