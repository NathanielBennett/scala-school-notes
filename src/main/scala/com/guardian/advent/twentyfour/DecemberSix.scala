package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, Cardinal, Direction, East, GridEntry, North, South, West}

sealed trait GridSpaceEntry extends GridEntry[Char]
case class Block(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry

trait EmptyEEntry extends GridSpaceEntry
case class Space(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends EmptyEEntry
case class Start(override val xPosition: Int, override val yPosition: Int, override val value: Char, cardinal: Cardinal) extends EmptyEEntry



object GridSpaceEntry {
   def apply(xPosition: Int, yPosition: Int, char: Char): Option[GridSpaceEntry] =
     char match {
       case '.' => Some(Space(xPosition, yPosition, char))
       case '>' => Some(Start(xPosition, yPosition, char, East))
       case '<' => Some(Start(xPosition, yPosition, char, West))
       case 'v' => Some(Start(xPosition, yPosition, char, South))
       case '^' => Some(Start(xPosition, yPosition, char, North))
       case '#' => Some(Block(xPosition, yPosition, char))
       case _ => None
     }
}

trait DecemberSixParser extends AdventOfCodeGridParser[Char, CharGrid] {
  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = GridSpaceEntry(x, y, value)
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberSix extends December[Int, CharGrid, GridEntry[Char]] with DecemberSixParser {

  override def day: Int = 6
  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  val grid = rawInput
  val begin = grid.entries
    .collectFirst {
      case start: Start => start
    }

  protected def verticeToBlock(verticeStart: GridEntry[Char], cardinal: Cardinal): List[GridEntry[Char]] = {
    grid.vertice(verticeStart, cardinal){ case (entry, entries) =>
      (entry :: entries).collectFirst {
        case block: Block => block
      }.isDefined
    }
  }

  protected def findVisitedSquares(verticeStart: GridEntry[Char], cardinal: Cardinal, visitedSoFar: List[GridEntry[Char]] = List.empty):  List[GridEntry[Char]] = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil => visitedSoFar
      case head :: tail =>
        val visited = (head :: tail) ::: visitedSoFar
        if ( grid.isEdge(head) ) visited
        else findVisitedSquares(head, cardinal.nextCardinal, visited)
    }
  }
}

trait DecemberSixPartOne extends DecemberSix {

  def findVisitedVertexes(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[(Direction, List[GridEntry[Char]])] ): List[(Direction, List[GridEntry[Char]])] = {
      verticeToBlock(verticeStart, cardinal) match {
        case Nil => acc
        case head :: tail =>
          val nextAcc = (cardinal, head :: tail) :: acc
          if( grid.isEdge(head) ) nextAcc
          else findVisitedVertexes(head, cardinal.nextCardinal, nextAcc)
      }
  }

  def checkVerticeForLoop(start: GridEntry[Char],  vertice: List[GridEntry[Char]], cardinal: Cardinal): Boolean = {
      vertice.headOption match {
        case None => false
        case Some(head) if grid.isEdge(head) => false
        case Some(head) =>
            val nextVertice = verticeToBlock(head, cardinal)
            nextVertice.contains(start) || checkVerticeForLoop(start, nextVertice, cardinal.nextCardinal)
          }
      }
  }

  def checkVerticeForLoops(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {
    val vertices =
  }

  override def rawSolution: List[GridEntry[Char]] =
     begin.map { start => findVisitedSquares(start, start.cardinal).toSet.toList }
      .getOrElse(List.empty)
}

trait DecemberSixPartTwo extends DecemberSix {

  def isLoop(maybeLoopStart: GridEntry[Char], verticeStart: GridEntry[Char], cardinal: Cardinal, seen: List[GridEntry[Char]]): Boolean = {
      verticeToBlock(verticeStart, cardinal) match {
        case Nil => false
        case head :: tail =>
          val visited = (head :: tail) ::: seen
          if ( head == maybeLoopStart) true
          else isLoop(maybeLoopStart, head, cardinal.nextCardinal, visited)
      }
  }

  override def rawSolution: List[GridEntry[Char]] = {

    begin.map { start =>
      val path = findVisitedSquares(start, start.cardinal).toSet
      path.filter{ entry =>
               
  }

}

object DecemberSixPartOneTest extends DecemberSixPartOne with PuzzleTest
object DecemberSixPartOneSolution extends DecemberSixPartOne with PuzzleSolution





/*
trait DecemberSixPartTwo extends DecemberSix {

  private def findLoops(entries: List[(GridEntry[Char], Cardinal)], acc: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
    entries match {
      case Nil => acc
      case head :: tail =>
        val (entry, cardinal) = head
        val nextCardinal = cardinal.nextCardinal
        val checkVertice = verticeToBlock(entry, nextCardinal)
        val nextAcc =  tail.find{ case (visitedEntry, vistedDirection) => checkVertice.contains(visitedEntry) && vistedDirection == nextCardinal.nextCardinal }
                .map { case(entry, _) => entry :: acc }.getOrElse(acc)

        findLoops(tail,nextAcc)
    }
  }

  override def rawSolution: List[GridEntry[Char]] = {
      begin.map{ start =>
         val allVisitedEntriesIndexed = findVisitedSquares(start, start.cardinal)
           .flatMap{ case (cardinal, entries) => entries.map{ entry => (entry, cardinal) } }
           .reverse

         findLoops(allVisitedEntriesIndexed )
      }.getOrElse(List.empty)
  }

  def rawSolutionXX: List[GridEntry[Char]] = {
      begin.flatMap {
         start =>
           val visited = findVisitedSquares(start, start.cardinal)
           val allVisitedGridEntries = visited.flatMap { case (cardinal, entries) =>
              entries.map{ case entry => (entry, cardinal) }
           }

          val loops = allVisitedGridEntries.zipWithIndex
           .map { case ((entry, cardinal), index ) => (entry, cardinal, index) }
           .groupBy{ case(entry, _, _) => entry }
           .filter{ case(_, entries) => entries.length == 2}
           .map{ case(entry, entries) => (entry, entries.sortBy { case(_, _, index) => index} )}
             .flatMap{ case (entry, entries) =>
               for {
                 (_, firstCardinal, _) <- entries.headOption
                 (_, maybeNextCardinal, _) <- entries.lastOption
               } yield (entry, firstCardinal.nextCardinal == maybeNextCardinal)
             }
             .filter{ case(_, loopPoint) => loopPoint }
             .map { case(entry, _) => entry }
             .toList

          allVisitedGridEntries.headOption.map{ case(h, _) =>  h :: loops }
      }.getOrElse(List.empty)
  }
}


object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest
*/
