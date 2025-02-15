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

  override def rawSolution: List[GridEntry[Char]] =
     begin.map { start => findVisitedSquares(start, start.cardinal).toSet.toList }
      .getOrElse(List.empty)
}

object DecemberSixPartOneTest extends DecemberSixPartOne with PuzzleTest
object DecemberSixPartOneSolution extends DecemberSixPartOne with PuzzleSolution


trait DecemberSixPartTwo extends DecemberSix {

  def findVisitedVertices(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[(List[GridEntry[Char]], Cardinal)] = List.empty): List[(List[GridEntry[Char]], Cardinal)] = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil => acc
      case head :: tail =>
        val nextAcc = (head :: tail, cardinal) :: acc
        if (grid.isEdge(head)) nextAcc
        else findVisitedVertices(head, cardinal.nextCardinal, nextAcc)
    }
  }


  def visitedWithDirection(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[List[(GridEntry[Char], Cardinal)]] = List.empty): List[List[(GridEntry[Char], Cardinal)]] = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil =>
        acc.reverse
      case head :: tail =>
        val nextAcc = (head :: tail).map { entry => (entry, cardinal) }.reverse :: acc
        if( grid.isEdge(head) ) nextAcc.reverse
        else visitedWithDirection(head, cardinal.nextCardinal, nextAcc)
    }
  }




  def isLoop(maybeLoopStart: (GridEntry[Char], Cardinal), visited: List[(GridEntry[Char], Cardinal)] ): Boolean = {

    val (startEntry, startCardinal) = maybeLoopStart
    val vertice = verticeToBlock(startEntry, startCardinal.nextCardinal) //Reverse?/
    vertice match {
      case Nil => false
      case head :: _ =>
        if (grid.isEdge(head)) false
        else visited.exists { case (entry, cardinwl) => entry.equalPosition(head) && cardinwl == startCardinal.nextCardinal } // Need tp recur || isLoop((head, startCardinal.nextCardinal), maybeLoopStart :: visited )
    }
  }

  def checkVerticeFour( path: List[(GridEntry[Char], Cardinal)], visited: List[(GridEntry[Char], Cardinal)], acc: List[GridEntry[Char]] ): List[GridEntry[Char]] = {
    path match {
      case Nil => acc
      case _ :: Nil => acc
      case  head :: next :: tail =>
        val nextAcc = if ( isLoop(next, visited) ) {
          val (loop, _) = head
          loop :: acc
        } else acc
        checkVerticeFour(next :: tail, visited, nextAcc)
    }
  }

  def checkPath(remainingPath: List[List[(GridEntry[Char], Cardinal)]], visited: List[(GridEntry[Char], Cardinal)] = List.empty, loops: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {
      remainingPath match {
        case Nil => loops
        case head :: tail =>
         val nextLoops = checkVerticeFour(head, visited, loops)
         checkPath(tail, head ::: visited, nextLoops)
      }
  }

  override def rawSolution: List[GridEntry[Char]] = {
    begin.map {
      start =>
          val verticesWithDirecttions = visitedWithDirection(start, start.cardinal)
          checkPath(verticesWithDirecttions) //503
    }.getOrElse(List.empty)
  }
}

object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest
object DecemberSixPartTwoSolution extends DecemberSixPartTwo with PuzzleSolution

object DecemberSixPartTwoDebug extends DecemberSixPartTwo with PuzzleTest with App {
  begin.map {
    start =>
      val visitedVertexesAndDirections = visitedWithDirection(start, start.cardinal)

      for {
        verticeDirections <- visitedVertexesAndDirections
        verticeWithDirection <- verticeDirections

      } yield {
        val (vertice, direction) = verticeWithDirection
        println(s"$vertice ($direction)")
      }

//      grid.printGridPathDebeg(start, visitedVertexesAndDirections)

  }
}


