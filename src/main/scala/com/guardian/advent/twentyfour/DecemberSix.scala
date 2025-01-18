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

/*
  def isLoop(maybeLoopStart: GridEntry[Char], verticeStart: GridEntry[Char], cardinal: Cardinal, seen: List[GridEntry[Char]]): Boolean = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil => false
      case head :: tail =>
        val visited = (head :: tail) ::: seen
        if (head == maybeLoopStart) true
        else isLoop(maybeLoopStart, head, cardinal.nextCardinal, visited)
    }
  }
*/

  def findVisitedVertices(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[(List[GridEntry[Char]], Cardinal)] = List.empty): List[(List[GridEntry[Char]], Cardinal)] = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil => acc
      case head :: tail =>
        val nextAcc = (head :: tail, cardinal) :: acc
        if (grid.isEdge(head)) nextAcc
        else findVisitedVertices(head, cardinal.nextCardinal, nextAcc)
    }
  }

  def checkVerticeForLoop(start: GridEntry[Char], vertice: List[GridEntry[Char]], cardinal: Cardinal): Boolean = {
    vertice.headOption match {
      case None => false
      case Some(head) if grid.isEdge(head) => false
      case Some(head) =>
        val nextVertice = verticeToBlock(head, cardinal)
        nextVertice.contains(start) || checkVerticeForLoop(start, nextVertice,  cardinal.nextCardinal)
    }
  }

  def checkVerticeEntryForLoop(verticeTail: List[GridEntry[Char]], cardinal: Cardinal, acc: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
    verticeTail match {
      case Nil => acc
      case _ :: Nil => acc
      case head :: tail =>
        val nextVertice = verticeToBlock(head, cardinal.nextCardinal)
        val isLoop = checkVerticeForLoop(head, nextVertice, cardinal.nextCardinal)
        //val nextAcc = if (isLoop) head :: acc else acc
        val nextAcc = if (isLoop) {
          println(s" headL $head F: $cardinal, N: ${cardinal.nextCardinal} ")
          grid.printGridDebug(head)
          println()
          head :: acc
        } else acc
//        else grid.nextEntryByDirection(head, cardinal.previousCardinal).map { e => e :: acc}.getOrElse(acc)
        checkVerticeEntryForLoop(tail, cardinal, nextAcc)
    }
  }

  def findLoops(start: Start): List[GridEntry[Char]] = {
    val vertices = findVisitedVertices(start, start.cardinal)
    vertices.reverse.flatMap { case(vertice, cardinal) =>
      println(s"v $vertices, $cardinal")
      checkVerticeEntryForLoop(vertice.tail, cardinal)
    }
  }

  override def rawSolution: List[GridEntry[Char]] = {
    begin.map {
      start =>
        val loops = findLoops(start)
//        loops.foreach(grid.printGridDebug(_))
        loops
    }.getOrElse(List.empty)
  }
}

object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest



