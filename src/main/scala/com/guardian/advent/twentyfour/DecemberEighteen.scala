package com.guardian.advent.twentyfour

import com.guardian.advent.December
import com.guardian.advent.grid.{Cardinal, CharEntry, CharGrid, GridEntry}
import com.guardian.advent.parsers.IntegerTupleParser
import com.guardian.advent.twentyfour.DecemberSixPartOneTest.cardinals

trait DecemberEighteenParser extends IntegerTupleParser {

  override def lineParser(line: String): Option[(Int, Int)] = {
    val l = line.toStringList(',')
    listToTuple(l)
  }
}

trait DecemberEighteen extends December[Int, List[(Int, Int)], GridEntry[Char]] with DecemberEighteenParser {

  protected val height: Int
  protected val width: Int
  protected val byteCnt: Int
  protected final val start = CharEntry(0, 0, '.')

  override def day: Int = 18

  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  protected def makeGrid(byteCount: Option[Int] = None): CharGrid = {

       val tuples = byteCount.map{ bytes => rawInput.take(bytes) }.getOrElse(rawInput)

       val corruptedBytes: Set[GridEntry[Char]] = (for {
         row <- 0 to height
         columnn <- 0 to  width
       } yield {
          val value = if(tuples.contains((columnn, row))) '#' else '.'
          CharEntry(columnn, row, value)
       }).toSet
       CharGrid(corruptedBytes)
  }

  private def nextStepsForPath(pathHead: GridEntry[Char], grid: CharGrid, seen: Set[GridEntry[Char]]): List[GridEntry[Char]] = {
    grid.getFilteredNeighbours(pathHead, cardinals) {
      entry => entry.value == '.' && !seen.contains(entry)
    }
  }

  private def nextStepsForPathSS(pathHead: GridEntry[Char], grid: CharGrid): List[GridEntry[Char]] = {
    grid.getFilteredNeighbours(pathHead, cardinals) {
      entry => entry.value == '.'
    }
  }

  private def isEnd(entry: GridEntry[Char]): Boolean = entry.equalPosition(width, height)

  protected def pathsThroughBytes(grid: CharGrid): List[List[GridEntry[Char]]] = {

    def nextSteps(currentPaths: List[List[GridEntry[Char]]], completedPaths: List[List[GridEntry[Char]]] = List.empty, seen: Set[GridEntry[Char]], cnt: Int = 0): List[List[GridEntry[Char]]] = {

      println(s"Loop $cnt. Current: ${currentPaths.size} Completed: ${completedPaths.size}")
      currentPaths match {
        case Nil => completedPaths
        case _ =>
          val m = currentPaths.flatMap{x => x}
          grid.printGridDebug(m, concat="", container = Some("0"))
          val toFold = (List[List[GridEntry[Char]]](), completedPaths, seen)
          val (updatedPaths, nextCompletedPaths, updatedSeen) = currentPaths.foldLeft(toFold) {
            case(folded, currentPath) =>
              val (currentPathsToUpdate, currentlyCompleted, currentlySeen) = folded
              currentPath match {
                case Nil => (currentPathsToUpdate, currentlyCompleted, currentlySeen)
                case head :: _ =>
                  val next = nextStepsForPathSS(head, grid)
                  next match {
                    case Nil => (currentPathsToUpdate, currentlyCompleted, currentlySeen)
                    case nextSteps =>
                      val (nextPaths, nextCompletedPaths) = nextSteps.find{ entry => isEnd(entry) }
                        .map { end =>
                          (
                            currentPathsToUpdate.filterNot{ path => path == currentPath },
                            (end :: currentPath) :: currentlyCompleted
                          )
                        }
                        .getOrElse {
                          val updatedPaths = nextSteps.filterNot{ e => currentlySeen.contains(e) }
                           .map{ next => next :: currentPath }
                          (updatedPaths, currentlyCompleted)
                        }
                        (nextPaths ++ currentPathsToUpdate, nextCompletedPaths, currentlySeen ++ nextSteps)
                  }
              }
          }
//          nextSteps(updatedPaths, nextCompletedPaths, updatedSeen, cnt + 1 )
          nextSteps(updatedPaths, nextCompletedPaths, updatedSeen, cnt + 1 )
      }
    }
    nextSteps(List(List(start)), seen = Set(start))
  }

/*  protected def pathsThroughBytesX(grid: CharGrid): List[List[GridEntry[Char]]] = {

    def nextSteps(currentPaths: List[List[GridEntry[Char]]], completedPaths: List[List[GridEntry[Char]]] = List.empty, seen: Set[GridEntry[Char]], cnt: Int = 0): List[List[GridEntry[Char]]] = {

      println(s"Loop $cnt. Current: ${currentPaths.size} Completed: ${completedPaths.size}")
      val groupedPaths = currentPaths.groupBy{ path => path.head }.toList
      if(groupedPaths.isEmpty) completedPaths
      else {
        val t = groupedPaths.flatMap {
          case (head, pathsForHead) =>
            val next = nextStepsForPath(head, grid, seen)
            next match {
              case Nil => (Nil, completedPaths)
              case nextSteps => nextSteps.find { entry => isEnd(entry) } match {
                  case Some(end) =>
                    val nowCompleted = pathsForHead.map { path => end :: path}
                    (List.empty, nowCompleted ::: completedPaths)
                  case _ =>
/*
                    val nextPaths = (for{
                      path <- pathsForHead
                      nextStep <- nextSteps
                    } yield nextStep :: path )
*/
                    val nextPaths = pathsForHead.flatMap{
                      path =>
                        nextSteps.map { nextStep => nextStep :: path}
                    }
                    (nextPaths, completedPaths)
                }
            }
        }



      }
      }

      println(s"Grouped: ${groupedPaths.size}")


    nextSteps(List(List(start)), seen = Set(start))
  }*/
}


trait DecemberEighteenPartOne extends DecemberEighteen {
  override def rawSolution: List[GridEntry[Char]] = {
     val grid = makeGrid(Some(byteCnt))
     val paths = pathsThroughBytes(grid)
     val m = paths.minBy(_.size)
     val all = paths.flatMap{ s => s }
     println(s"Paths ${paths.size}")
     grid.printGridDebug(all, concat="", container = Some("0"))
     m
  }
}

trait DecemberEighteenTest extends DecemberEighteen {
  override protected val height: Int = 6
  override protected val width: Int = 6
  override protected val byteCnt: Int = 12
}

trait DecemberEighteenSolution extends DecemberEighteen {
  override protected val height: Int = 70
  override protected val width: Int = 70
  override protected val byteCnt: Int = 1024
}

object DecemberEighteenPartOneTest extends DecemberEighteenPartOne with PuzzleTest with DecemberEighteenTest
object DecemberEighteenPartOneSolution extends DecemberEighteenPartOne with PuzzleSolution with DecemberEighteenSolution

