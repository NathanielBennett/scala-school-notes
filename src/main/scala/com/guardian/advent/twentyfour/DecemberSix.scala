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

/*
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
      case  Some(head) =>
        val nextVertice = verticeToBlock(head, cardinal)
        val foundLoop = nextVertice.size > 1 &&  nextVertice.contains(start)
        if (foundLoop) println(s"sVert $start.($cardinal) v $nextVertice: Good: ${nextVertice.size > 1}" )
        foundLoop || checkVerticeForLoop(start, nextVertice,  cardinal.nextCardinal)
    }
  }

  def checkVerticeForLoopTwo(start: GridEntry[Char], vertice: List[GridEntry[Char]], cardinal: Cardinal): Boolean = {
     vertice match {
       case Nil => false
       case _ :: Nil => false
       case head :: _ if (grid.isEdge(head)) => false
       case head :: tail =>
         val nextVertice = verticeToBlock(head, cardinal)
         val foundLoop = nextVertice.contains(start)
         foundLoop || checkVerticeForLoopThree(start, nextVertice, cardinal.nextCardinal)
     }
  }

  def checkVerticeForLoopThree(start: GridEntry[Char], vertice: List[GridEntry[Char]], cardinal: Cardinal): Boolean = {
    vertice match {
      case Nil => false
      case _ :: Nil => false
      case head :: _ if(grid.isEdge(head)) => false
      case head :: tail =>
        val foundLøop = vertice.contains(start)
        foundLøop || checkVerticeForLoopTwo(start, verticeToBlock(head, cardinal.nextCardinal), cardinal.nextCardinal)

    }
  }

  def checkVerticeEntryForLoop(verticeTail: List[GridEntry[Char]], cardinal: Cardinal, acc: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
    verticeTail match {
      case Nil => acc
      case _ :: Nil => acc
      case head :: tail =>
        val verticeToCheck = verticeToBlock(head, cardinal.nextCardinal)
        val isLoop = checkVerticeForLoopTwo(head, verticeToCheck, cardinal.nextCardinal)
        //val nextAcc = if (isLoop) head :: acc else acc
        val nextAcc = if (isLoop) {
//          println(s" headL $head F: $cardinal, N: ${cardinal.nextCardinal} ")
  //        grid.printGridDebug(head)
          println("+")
          head :: acc
        } else acc
//        else grid.nextEntryByDirection(head, cardinal.previousCardinal).map { e => e :: acc}.getOrElse(acc)
        checkVerticeEntryForLoop(tail, cardinal, nextAcc)
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

  def findLoops(vertices: List[(List[GridEntry[Char]], Cardinal)], visited: List[(GridEntry[Char], Cardinal)] = List.empty, loops: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
      vertices match {
        case Nil => loops
        case head :: tail =>
          val (vertice, cardinal) = head
          val maybeLoops = (for{
            entry <- vertice
            (loopStart, _) <- visited.find{ case(gridEntry, entryCardinal) => entry.equalPosition(gridEntry) && cardinal.nextCardinal == entryCardinal }
            obstruction <- grid.nextEntryByDirection(loopStart, cardinal)
          } yield obstruction )

          val nextVisted = vertice.map{ entry => (entry, cardinal)} ::: visited
          findLoops(tail, nextVisted, maybeLoops ::: loops )
      }
  }

  def checkVerticeEntriesForLoop( baseVertice: List[GridEntry[Char]], verticeCardinal: Cardinal, checkCardinal: Cardinal, alreadyVisited: List[(GridEntry[Char], Cardinal)], loopAcc: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
    baseVertice match {
      case Nil => loopAcc
      case _ :: Nil => loopAcc
      case head :: next :: tail =>
        println(s"============ ${baseVertice.length}")
        val nowVisited = (head, verticeCardinal) :: alreadyVisited
        val isNextLoop = checkVerticeForLoop(next, checkCardinal, nowVisited)
//        println(s"F: $isNextLoop")
        val nextLoopAcc = if (isNextLoop) head :: loopAcc else loopAcc
        checkVerticeEntriesForLoop(next :: tail, verticeCardinal, checkCardinal, nowVisited, nextLoopAcc)
    }
  }

  def checkVerticeForLoop(start: GridEntry[Char], cardinal: Cardinal, alreadyVisited: List[(GridEntry[Char], Cardinal)], seenCnt: Int = 0): Boolean = {
   // println(s"Visited: ${alreadyVisited.size}")
    println(s"start: X: $start, C: $cardinal: Seen: $seenCnt, Visited@ ${alreadyVisited.size}")

    val vertice = verticeToBlock(start, cardinal)
    //if (bool) println(s"start: $start, vertice: ${vertice} ")
    vertice match {
      case Nil =>
        println("Gotcha")
        false
      case head :: tail =>
/*
        if(bool) {
          println(s"IsEdge: ${grid.isEdge(head)}")
          println(s"Seen: ${alreadyVisited.contains(head, cardinal)}}")
        }
*/
//        println(s"$head: E; $tail")
        if (grid.isEdge(head)) false
        else alreadyVisited.contains((head, cardinal)) || checkVerticeForLoop(head, cardinal.nextCardinal, (head, cardinal) :: alreadyVisited, seenCnt + 1 )
    }
  }

  def makeAlreadyVisited(vertices: List[(List[GridEntry[Char]], Cardinal)], visited: List[(GridEntry[Char], Cardinal)] = List.empty, cnt: Int = 0): List[(GridEntry[Char], Cardinal)] =
    if(cnt > 6) visited
    else {
      val (vertice, cardinal) = vertices.head
      makeAlreadyVisited(vertices.tail, vertice.map{case entry => (entry, cardinal)} ::: visited, cnt + 1)
    }

  def findBlocksOnPath(vistedVertices: List[(List[GridEntry[Char]], Cardinal) ] ): List[(Block, Cardinal)] = {
      vistedVertices
      .flatMap{ case (vertice, cardinal) =>
          grid.nextEntryByDirection(vertice.head, cardinal)
            .map{ case next => (next, cardinal) }
            .collect {
              case (block: Block, cardinal: Cardinal) => (block, cardinal)
            }
      }
  }
  def checkVertice(vertice: List[GridEntry[Char]], cardinal: Cardinal, seenBlocks: List[Block], loopAcc: List[GridEntry[Char]] ): List[GridEntry[Char]] = {
    def entryMatches(gridEntry: GridEntry[Char]): Boolean = gridEntry.isInstanceOf[Block]

    vertice match {
      case Nil => loopAcc
      case head :: tail =>
        grid.nextMatchingEntry(head, cardinal, entryMatches) match {
          case Some(block)  =>
            val nextAcc = if (seenBlocks.contains(block)) block :: loopAcc else loopAcc
            checkVertice(tail, cardinal, seenBlocks, nextAcc)
        }
    }
  }

  override def rawSolution: List[GridEntry[Char]] = {
    begin.map {
      start =>
        val visitedVertexesAndDirections = findVisitedVertices(start, start.cardinal).reverse
        val blocksAndDirections = findBlocksOnPath(visitedVertexesAndDirections)



    }
    List.empty
  }


   def rawSolutionX2: List[GridEntry[Char]] = {
    begin.map {
      case start =>
   //     println(s"Start: $start")
        val verticesAndCardinals = findVisitedVertices(start, start.cardinal).reverse
        val alreadyVisited: List[(GridEntry[Char], Cardinal)] = List.empty
        val loopAcc: List[GridEntry[Char]] = List.empty
        val (loops, _) = verticesAndCardinals.foldLeft((loopAcc, alreadyVisited)) {
          case ((loopAcc, visited), verticeCardinal) =>
            //println(s"loops: $verticeCardinal")
            val (vertice, cardinal) = verticeCardinal
            val newLoops = checkVerticeEntriesForLoop(vertice, cardinal, cardinal.nextCardinal, visited) ::: loopAcc
            println("+++++++")
            //println(s"new ${newLoops}")
            val newVisted = vertice.map{ case entry => (entry, cardinal)} ::: visited
            (newLoops, newVisted)
        }
        loops
      }.getOrElse(List.empty)
    }


/*
    def rawSolutionX: List[GridEntry[Char]] = {
      begin.map {
        start =>
          val vertices = findVisitedVertices(start, start.cardinal).reverse
          val alreadyVisited = makeAlreadyVisited(vertices)
          val (vertice, cardinal) = vertices(7)
          val c = checkVerticeEntriesForLoop(vertice, cardinal.nextCardinal, alreadyVisited )
     //     println(c)
          List.empty
        //findLoops(vertices.reverse)
      }.getOrElse(List.empty)
    }
*/

}

object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest
object DecemberSixPartTwoSolution extends DecemberSixPartTwo with PuzzleSolution

object DecemberSixPartTwoDebug extends DecemberSixPartTwo with PuzzleSolution with App {
  grid.findEntry(106, 37).map {
     entry =>
      val vertice = verticeToBlock(entry, East)
      println(vertice)
      println(grid.isEdge(vertice.head))
  }
}


