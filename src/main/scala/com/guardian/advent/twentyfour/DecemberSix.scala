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

  protected def findVisitedSquares(verticeStart: GridEntry[Char], cardinal: Cardinal, visitedSoFar: List[(Cardinal, List[GridEntry[Char]])] = List.empty):
   List[(Cardinal, List[GridEntry[Char]])] = {

    val vertice = grid.vertice(verticeStart, cardinal) {
      (entry, list) => (entry :: list).collectFirst {
          case block: Block => block
        }.isDefined
      }

    vertice match {
      case Nil => visitedSoFar
      case head :: _ =>
        val visited = (cardinal, vertice) :: visitedSoFar
        if ( grid.isEdge(head) ) visited
        else findVisitedSquares(head, cardinal.nextCardinal, visited)
    }
  }
}

trait DecemberSixPartOne extends DecemberSix {

  override def rawSolution: List[GridEntry[Char]] = {
     begin.map {
        start => findVisitedSquares(start, start.cardinal).flatMap {
          case (_, entries) => entries
        }.toSet.toList
     }
  }.getOrElse(List.empty)
}

trait DecemberSixPartTwo extends DecemberSix {

  override def rawSolution: List[GridEntry[Char]] = {
      begin.map {
         start =>
           val visited = findVisitedSquares(start, start.cardinal)
           val vistedMoreThanOnce = visited.flatMap { case (cardinal, entries) =>
              entries.map{ case entry => (entry, cardinal) }
           }


          val loopPO.zipWithIndex
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
             .filter{ case(_, loopPoint) => ;loopPoint }
             .map { case(entry, _) => entry }
             .toList


          visited.headOption.map{ h => h :: vistedMoreThanOnce }.getOrElse(vistedMoreThanOnce)
      }.getOrElse(List[GridEntry[Char]]())
  }
}

object DecemberSixPartOneTest extends DecemberSixPartOne with PuzzleTest
object DecemberSixPartOneSolution extends DecemberSixPartOne with PuzzleSolution

object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest