package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, Cardinal, Direction, East, GridEntry, North, South, West}

sealed trait GridSpaceEntry extends GridEntry[Char]
case class Block(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry

trait EmptyEntry extends GridSpaceEntry
case class Space(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends EmptyEntry
case class Start(override val xPosition: Int, override val yPosition: Int, override val value: Char, cardinal: Cardinal) extends EmptyEntry

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

trait
DecemberSixParser extends AdventOfCodeGridParser[Char, CharGrid] {
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

  protected def verticeToBlock(verticeStart: GridEntry[Char],  cardinal: Cardinal): List[GridEntry[Char]] = {
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
     begin.map { start =>
       findVisitedSquares(start, start.cardinal).toSet.toList }
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
        val nextAcc = (head :: tail).reverse.map { entry => (entry, cardinal) } :: acc
        if( grid.isEdge(head) ) nextAcc.reverse
        else visitedWithDirection(head, cardinal.nextCardinal, nextAcc)
    }
  }

  def isLoop(maybeLoopStart: (GridEntry[Char], Cardinal), maybeBlock: GridEntry[Char], visited: List[(GridEntry[Char], Cardinal)] ): Boolean = {

    def filterVerticeByVisited(entry: GridEntry[Char],  cardinal: Cardinal): Boolean =
        visited.exists{ case(visitedEntry, visitedCardinal) => entry.equalPosition(visitedEntry)  &&
          Set(cardinal, cardinal.counterCardinal).contains(visitedCardinal) }

    def visitedContainsVertice(verticeToCheck: List[GridEntry[Char]], cardinalToCheck: Cardinal): Boolean = verticeToCheck.map{ entry => (entry, cardinalToCheck)}.intersect(visited).nonEmpty

    val (startEntry, cardinal, cardinalForVertice) = {
      val (entry, cardinal) = maybeLoopStart
      (entry, cardinal, cardinal.nextCardinal)
    }
//    println(s"Loop $startEntry, $maybeBlock")

    val vertice = verticeToBlock(startEntry, cardinalForVertice)

    vertice match {
      case Nil => false
      case head :: _ =>
        if( vertice.contains(maybeBlock)) true
        else if (grid.isEdge(head) || filterVerticeByVisited(maybeBlock, cardinal)) false
        else visitedContainsVertice(vertice, cardinalForVertice) ||
          isLoop((head, cardinalForVertice), maybeBlock, vertice.map { entry => (entry, cardinalForVertice) } ::: (maybeLoopStart :: visited))
    }
  }

  def checkVerticeFour( path: List[(GridEntry[Char], Cardinal)], visited: List[(GridEntry[Char], Cardinal)], acc: List[GridEntry[Char]] ): List[GridEntry[Char]] = {
    //println(s"Path ${path.size}")
    path match {
      case Nil => acc
      case _ :: Nil => acc
      case  head :: next :: tail =>
        val (nextEntry, _) = next
        val found = isLoop(head, nextEntry, visited)
        val nextAcc = if ( found ) {
      //    println(next)
          nextEntry :: acc
        } else acc
        checkVerticeFour(next :: tail, head :: visited, nextAcc)
    }
  }

  def checkPath(remainingPath: List[List[(GridEntry[Char], Cardinal)]], visited: List[(GridEntry[Char], Cardinal)] = List.empty, loops: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {

    def filterVerticeByVisited(vertice: List[(GridEntry[Char], Cardinal)]): List[(GridEntry[Char], Cardinal)] = {
      vertice.filterNot{ case (entry, cardinal) =>
        visited.exists{ case(visitedEntry, visitedCardinal) => entry.equalPosition(visitedEntry)  &&
          Set(cardinal, cardinal.counterCardinal).contains(visitedCardinal) } }
    }

    remainingPath match {
        case Nil => loops
        case head :: tail =>
        // println(head)
         val unvisitedInCurrentVertice = filterVerticeByVisited(head)
         val nextLoops = checkVerticeFour(unvisitedInCurrentVertice, visited, loops)
         checkPath(tail, head ::: visited, nextLoops)
      }
  }

  override def rawSolution: List[GridEntry[Char]] = {
    begin.map {
      start =>
        val verticesWithDirecttions = visitedWithDirection(start, start.cardinal)
        val loops = checkPath(verticesWithDirecttions) //503
/*
        println(loops)
        println(loops.size)
        println(loops.toSet.toList.size)
*/
        grid.printGridDebug(loops, "")
        loops.toSet.toList
    }.getOrElse(List.empty)
  }
}

object DecemberSixPartTwoTest extends DecemberSixPartTwo with PuzzleTest
object DecemberSixPartTwoSolution extends DecemberSixPartTwo with PuzzleSolution

trait DecemberSixPartTwoRefactor extends DecemberSix  {

  type Vertice = List[GridEntry[Char]]
  type BlockMap = Map[GridEntry[Char], List[Cardinal]]

  private def updateBlockMap(blockMap: BlockMap, block: GridEntry[Char], cardinal: Cardinal): BlockMap = {
    val cardinals = blockMap.get(block).map {cardinals => cardinal :: cardinals}.getOrElse(List(cardinal))
    blockMap ++ Map(block -> cardinals)
  }

  private def  nextBlockInLoopTest(vertice: Vertice, cardinal: Cardinal, blockMap: BlockMap): BlockMap = {
    blockMap.keys.toList.intersect(vertice) match {
      case Nil => blockForVertice(vertice, cardinal).map{ block => updateBlockMap(blockMap, block, cardinal)}.getOrElse(blockMap)
      case head :: _ => updateBlockMap(blockMap, head, cardinal)
    }
  }

  private def blockForVertice(vertice: Vertice, cardinal: Cardinal): Option[Block] = {
    for {
      verticeEnd <- vertice.reverse.lastOption
      block <- grid.nextEntryByDirection(verticeEnd, cardinal).collectFirst {
        case block: Block => block
      }
    } yield block
  }

  def findVisitedVertices(verticeStart: GridEntry[Char], cardinal: Cardinal, acc: List[(List[GridEntry[Char]], Cardinal)] = List.empty): List[(List[GridEntry[Char]], Cardinal)] = {
    verticeToBlock(verticeStart, cardinal) match {
      case Nil => acc.reverse
      case head :: tail =>
        val nextAcc = ((head :: tail).reverse, cardinal) :: acc
        if (grid.isEdge(head)) nextAcc.reverse
        else findVisitedVertices(head, cardinal.nextCardinal, nextAcc)
    }
  }



  def isLoop(startEntry: GridEntry[Char], cardinal: Cardinal, maybeBlock: GridEntry[Char], blockMap: BlockMap): Boolean = {

     val vertice = verticeToBlock(startEntry, cardinal)

     vertice match {
       case Nil => false
       case head :: _ =>
        if (vertice.contains(maybeBlock)) true
        else if ( grid.isEdge(head) ) false
        else {
          val maybeLoop = (for {
            block <- blockForVertice(vertice, cardinal)
            hitPreviously <- blockMap.get(block)
          } yield  hitPreviously.contains(cardinal) ).getOrElse(false)

          maybeLoop || {
            val newBlockMap = blockForVertice(vertice, cardinal).map{ block => updateBlockMap(blockMap, block, cardinal)}.getOrElse(blockMap)
            isLoop(head, cardinal.nextCardinal, maybeBlock, newBlockMap)
          }
        }
     }
  }

  def isLoopC(start: (GridEntry[Char], Cardinal), maybeBlock: (GridEntry[Char], Cardinal), blockMap: BlockMap): Boolean = {

    val (startEntry, cardinal) = start
    val (blockEntry, blockCardinal ) = maybeBlock

     val vertice = verticeToBlock(startEntry, cardinal)
       .reverse
       .takeWhile{ entry => !entry.equalPosition(blockEntry) }
       .reverse

     vertice match {
       case Nil => false
       case head :: _ =>
           if (grid.isEdge(head)) false
           else {
             val maybeVerticeBlock = vertice.contains(blockEntry).toOption[GridEntry[Char]](blockEntry)
               .orElse(blockForVertice(vertice, cardinal))

             val isLoop = maybeVerticeBlock.flatMap { bl => blockMap.get(bl) }
               .getOrElse(List.empty)
               .contains(cardinal)

             isLoop || {
               val nextBlockMap = maybeVerticeBlock.map { bl => updateBlockMap(blockMap, bl, cardinal) }.getOrElse(blockMap)
               isLoopC((head, cardinal.nextCardinal), maybeBlock, nextBlockMap)
             }
           }
        }

  }


  def checkVertice(
                    vertice: Vertice, cardinal: Cardinal,
                    blockMap: BlockMap,
                    visited: List[(GridEntry[Char], Cardinal)],
                    loopsForVertice: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {

     def isMaybeBlock(gridEntry: GridEntry[Char], maybeCardinal: Cardinal): Boolean = {
        !gridEntry.isInstanceOf[Start] &&
       !visited.exists{ case (visitedEntry, visitedCardinal) => gridEntry.equalPosition(visitedEntry) }// && Set(maybeCardinal, maybeCardinal.counterCardinal).contains(visitedCardinal)  }
     }

     vertice match {
       case Nil => loopsForVertice
       case _ :: Nil => loopsForVertice
       case head :: next :: tail =>
          val isThisLoop = isMaybeBlock(next, cardinal) && isLoopC((head, cardinal.nextCardinal),(next, cardinal), blockMap)
          val nextLoops = if(isThisLoop) next :: loopsForVertice else loopsForVertice
          checkVertice(next :: tail, cardinal, blockMap, (head, cardinal) :: visited, nextLoops) //
     }
  }

  def checkPath(remainingPath: List[(Vertice, Cardinal)], blockMap: BlockMap = Map.empty, visited: List[(GridEntry[Char], Cardinal)] = List.empty, loops: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {

    println(s"vertices remaining: ${remainingPath.size}")

    remainingPath match {
      case Nil => loops
      case head :: tail =>
        val (vertice, cardinal) = head
        val nextLoops = checkVertice(vertice, cardinal, blockMap, visited ) ::: loops
        val nextBlockMap = blockForVertice(vertice, cardinal).map { block => updateBlockMap(blockMap, block, cardinal) }.getOrElse(blockMap)
        val nextVisited = vertice.map{ entry => (entry, cardinal) } ::: visited
        checkPath(tail, nextBlockMap,  nextVisited, nextLoops)
    }
  }

  def debugVertices(vertices: List[(Vertice, Cardinal)]) = vertices.foreach { case(vertice, cardinal) => println(s"$vertice ($cardinal)")}
//

  override def rawSolution: List[GridEntry[Char]] = {
    begin.map {
      start =>
        grid.printGrid()
        val verticeData = findVisitedVertices(start, start.cardinal)
        debugVertices(verticeData)

       val loops = checkPath(verticeData)
        println(s"Loops: ${loops.size}(${loops.toSet.size})")
        grid.printGridDebug(loops)
        loops
    }.get
  }
}

object RefactorTest extends DecemberSixPartTwoRefactor with App with PuzzleTest {
  rawSolution
}
object RefactorSolution extends DecemberSixPartTwoRefactor with App with PuzzleSolution {
  rawSolution
}

class DecemberSixEdgeCaseTester(fileIndex: Int) extends DecemberSixPartTwoRefactor with PuzzleTest {
  override lazy val resourceName: String = s"debug/day_6_debug_${fileIndex}.txt"
}

object RefactorDebug extends DecemberSixPartTwoRefactor  with App with PuzzleTest {
   override lazy val resourceName: String = "debug/day_6_debug_16.txt"
  rawSolution
}



//class DecemberSixPartTwoDebug(override val resourceName: String) extends DecemberSixPartTwo with PuzzleTest with App {
class DecemberSixPartTwoDebug(override val debugCase: Int)  extends DecemberSixPartTwo with PuzzleDebugger {

  //1, 1, 2 1, 3 2, 4 0 db 19 6:0

  //11: 6 12 1 13 0
  /*  begin.map {
    start =>
      val verticesWithDirecttions = visitedWithDirection(start, start.cardinal)



      val loops = checkPath(verticesWithDirecttions) //503
      println(s"Loops: ${loops.size}")
      println(s"Loops: ${loops}")
      grid.printGridDebug(loops)
  }*/
  override def rawSolution: List[GridEntry[Char]] = {
    val blocks = super.rawSolution
    grid.printGridDebug(blocks, "")
    println(s"(${blocks.length})")
    println()
    blocks
  }

  override def test: Boolean = true
}

class DecemberSixPartTwoDebugRefactor(override val debugCase: Int)  extends DecemberSixPartTwoRefactor with PuzzleDebugger {

  //1, 1, 2 1, 3 2, 4 0 db 19 6:0

  //11: 6 12 1 13 0
  /*  begin.map {
    start =>
      val verticesWithDirecttions = visitedWithDirection(start, start.cardinal)



      val loops = checkPath(verticesWithDirecttions) //503
      println(s"Loops: ${loops.size}")
      println(s"Loops: ${loops}")
      grid.printGridDebug(loops)
  }*/
  override def rawSolution: List[GridEntry[Char]] = {
    val blocks = super.rawSolution
    grid.printGridDebug(blocks, "")
    println(s"(${blocks.length})")
    println()
    blocks
  }

  override def test: Boolean = true
}

