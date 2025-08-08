package com.guardian.advent.twentyfour

import com.guardian.advent.twentyfour.util.DecSixDiffer
import com.guardian.advent.{Cardinal, GridEntry, North}

import java.io.{File, PrintWriter}
import scala.io.Source

trait DecemberSixRefactor extends December[Int, CharGrid, GridEntry[Char]] with DecemberSixParser {

  type PathEntry = (GridEntry[Char], Cardinal)
  type BlockMap = Map[GridEntry[Char], List[Cardinal]]

  override def day: Int = 6
  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  val grid = rawInput
  val begin = grid.entries.collectFirst{
    case start: Start => start
  }

  protected def verticeToBlock(start: GridEntry[Char], cardinal: Cardinal, blockMap: BlockMap = Map.empty): List[PathEntry] = {
    grid.vertice(start, cardinal) { case (entry, entries) =>
        blockMap.keySet.contains(entry) || (entry :: entries).collectFirst {
          case block: Block => block
        }.isDefined
    }
    .map{ entry => (entry, cardinal)}
  }

  protected def nextVerticeStart(gridEntry: GridEntry[Char], cardinal: Cardinal): Option[PathEntry] = {
     if( grid.isEdge(gridEntry)) None
     else {
       val maybeSpace = grid.nextEntryByDirection(gridEntry, cardinal).collectFirst {
         case emptyEntry: EmptyEntry => emptyEntry
       }
       maybeSpace.map { space => (space, cardinal) }.orElse(
         nextVerticeStart(gridEntry, cardinal.nextCardinal)
       )
     }
  }

  protected def walkPath ( maybeVerticeStart: Option[PathEntry], pathSoFar: List[PathEntry] ): List[PathEntry] = {
    maybeVerticeStart.map {
      case(entry, cardinal) =>
        val vertice = verticeToBlock(entry, cardinal)
        val maybeNextVerticeStart = vertice.headOption.flatMap {
          case (head, cardinal) => nextVerticeStart(head, cardinal.nextCardinal)
        }
        walkPath(maybeNextVerticeStart, vertice ::: pathSoFar)
    }.getOrElse(pathSoFar.reverse)
  }

  protected def findPath: List[PathEntry] = begin.map { start =>
    val firstStart = grid.nextEntryByDirection(start, start.cardinal).map { entry => (entry, start.cardinal) }
       val path = walkPath(firstStart, List((start, start.cardinal)))
       //grid.printGridPathDebug(start, path)
       path
   }.getOrElse(List.empty)
}

trait DecemberSixRefactorPartOne extends DecemberSixRefactor {
  override def rawSolution: List[GridEntry[Char]] = {
    findPath.map{ case(entry, _) => entry}
      .toSet
      .toList
  }
}

class TestGridWalker( override val grid: CharGrid) extends DecemberSixRefactorPartOne with PuzzleSolution {}

object DecemberSixRefactorPartOneTest extends DecemberSixRefactorPartOne with PuzzleTest
object DecemberSixRefactorPartOneSolution extends DecemberSixRefactorPartOne with PuzzleSolution

trait DecemberSixRefactorPartTwo extends DecemberSixRefactor {

  implicit def pathToVertice(path: List[PathEntry]): List[GridEntry[Char]] = path.map{ case(entry, _) => entry}

  private def debugPathSize(path: List[PathEntry]): Unit = {
    println(s"Raw path size: ${path.size}")
    println(s"Deduped = ${pathToVertice(path).toSet.size}")
  }

  private def getCurrentVertice(path: List[PathEntry]): (List[PathEntry], List[PathEntry]) = {
    path.headOption.map {
      case (_, currentCardinal) =>
        val currentVertice = path.takeWhile { case (_, cardinal) => cardinal == currentCardinal }
        val remainingPath = path.drop(currentVertice.length)
        (currentVertice, remainingPath)
    }.getOrElse((List.empty, List.empty))
  }

  private def updateBlockMap(blockMap: BlockMap, pathEntry: PathEntry): BlockMap = {
    val (block, cardinal) = pathEntry
    val cardinals = blockMap.get(block).map{ cardinals => cardinal :: cardinals }.getOrElse(List(cardinal))
    blockMap ++ Map(block -> cardinals)
  }

  private def blocksForVertice(pathEntry: PathEntry, blockMap: BlockMap): BlockMap = {
    val (gridEntry, cardinal) = pathEntry
    grid.nextEntryByDirection(gridEntry, cardinal) match {
      case Some(block: Block) =>
          val nextBlockMap = updateBlockMap(blockMap, (block, cardinal))
          blocksForVertice((gridEntry, cardinal.nextCardinal),nextBlockMap)
      case _  => blockMap
    }
  }

  //TODO move blokmap methods to class
  private def  updateBlockMapForVertice(head: PathEntry, blockMap: BlockMap): BlockMap = {

    //TODO - squeeze dawn?
     val (headEntry, cardinal) = head

     getBlocksForVertice(headEntry, cardinal).foldRight(blockMap) {
          case(pathEntry, blockMap) => updateBlockMap(blockMap, pathEntry)
      }
  }

  //Vertice class
  private def blockForVertice(vertice: List[GridEntry[Char]], cardinal: Cardinal): Option[GridEntry[Char]] = {
     vertice.headOption.flatMap{ verticeHead => grid.nextEntryByDirection(verticeHead, cardinal)}
  }

  def getBlocksForVertice(lastHead: GridEntry[Char], cardinal: Cardinal, blocksEntries: List[PathEntry] = List.empty):  List[PathEntry] = {
    grid.nextEntryByDirection(lastHead, cardinal) match {
      case Some(block: Block) => getBlocksForVertice(lastHead, cardinal.nextCardinal, (block, cardinal) :: blocksEntries)
      case _ => blocksEntries
    }
  }

  //Not Path Entry
  private def checkLoop(maybeVerticeStart: Option[PathEntry], blockMap: BlockMap): Boolean = {


    def blockHitPreviouslyF(vertice: List[PathEntry], cardinal: Cardinal): Boolean = {
      (for{
        blockEntry <- blockForVertice(vertice, cardinal)
        cardinalsForBlock <- blockMap.get(blockEntry)
      } yield cardinalsForBlock.contains(cardinal)).getOrElse(false)
    }

    maybeVerticeStart.map {
      verticeStart =>

        val (startEntry, cardinal) = verticeStart
        val vertice = verticeToBlock(startEntry, cardinal, blockMap)

        vertice match {
          case Nil => false
          case head :: _ =>
            val isLoop = blockHitPreviouslyF(vertice, cardinal)

            isLoop || {
              val nextBlockMap = updateBlockMapForVertice(head, blockMap)
              val maybeNextVerticeStart = vertice.headOption.flatMap {
                head => nextVerticeStart(head._1, cardinal.nextCardinal)
              }
              checkLoop(maybeNextVerticeStart, nextBlockMap)
            }
        }
    }.getOrElse(false)
  }

  def checkVertice(vertice: List[PathEntry], visited: List[PathEntry], blockMap: BlockMap, blocks: List[PathEntry] = List.empty): List[PathEntry] = {

    def isBlockCandidate(entry: PathEntry): Boolean = visited.doesNotContain(entry)
    //1934
    def isBlockCandidate2(entry: GridEntry[Char]): Boolean = visited.map{ case(entryk, _) => entryk}.doesNotContain(entry)

    vertice match {
      case head :: next :: tail =>
        val (headEntry, cardinal) = head
        lazy val loopStart = (headEntry, cardinal.nextCardinal)
        val isThisLoop = isBlockCandidate2(next._1) && {
          val blockMapWithBlockCandidate = updateBlockMap(blockMap, next)
          checkLoop(Some(loopStart), blockMapWithBlockCandidate)
        }
        val nextBlocks = if(isThisLoop) next :: blocks else blocks
        checkVertice(next :: tail, head :: visited, blockMap, nextBlocks)
      case _ =>
        blocks
    }
  }


  def findBlocks( pathRemaining: List[PathEntry], visited: List[PathEntry] = List.empty, blockMap: BlockMap = Map.empty, blocks: List[PathEntry] = List.empty ): List[PathEntry] = {
     debugPathSize(pathRemaining)
     pathRemaining match {
       case Nil =>
         blocks
       case _ :: _ =>
         val (currentVertice, nextPathRemaining) = getCurrentVertice(pathRemaining)
         val blocksForVertice = checkVertice(currentVertice, visited, blockMap)
         val nextBlockMap =
           currentVertice.reverse.headOption.map{ head => updateBlockMapForVertice(head, blockMap) }.getOrElse(blockMap)
         findBlocks(nextPathRemaining, currentVertice ::: visited, nextBlockMap, blocksForVertice ::: blocks)
     }
  }

  override def rawSolution: List[GridEntry[Char]] = {
     val path = findPath
     val raw = findBlocks(path.tail)
     writeResults(raw)
    /*
         grid.printGridDebug(raw, "")
         println()
          println(s"L: ${raw.size}")
    */
     raw
  }

  def writeResults(entries: List[PathEntry]): Unit = {

      entries.zipWithIndex.foreach{ case(bl, index) => println(s"($index) $bl") }
  }
}

object DecemberSixRefactorPartTwoTest extends DecemberSixRefactorPartTwo with PuzzleTest
object DecemberSixRefactorPartTwoSolution extends DecemberSixRefactorPartTwo with PuzzleSolution

class DecemberSixRefactorPartTwoDebugger(override val debugCase: Int) extends DecemberSixRefactorPartTwo with PuzzleDebugger {
  override def test: Boolean = true

  override def rawSolution: List[GridEntry[Char]] = {
//    grid.printGrid()
    super.rawSolution
  }
}

object DecemberSixPartTwoRefactorDebugger extends DecemberSixRefactorPartTwo with PuzzleSolution  {
  override def test: Boolean = false

  def walkBlockedPath ( maybeVerticeStart: Option[PathEntry], pathSoFar: List[PathEntry], blockMap: BlockMap ): List[PathEntry] = {
    maybeVerticeStart.map {
      case(entry, cardinal) =>
        val vertice = verticeToBlock(entry, cardinal, blockMap)
        val maybeNextVerticeStart = vertice.headOption.flatMap {
          case (head, cardinal) => nextVerticeStart(head, cardinal.nextCardinal)
        }
        walkBlockedPath(maybeNextVerticeStart, vertice ::: pathSoFar, blockMap)
    }.getOrElse(pathSoFar.reverse)
  }

  def findMissing(path: List[PathEntry], missing: List[PathEntry], seen: List[PathEntry] = List.empty, last: PathEntry = (Space(-1, -1, '.'), North), cnt: Int = 1): Unit ={
    path match {
      case Nil =>
        println("Done")
      case head :: tail =>
        if (missing.contains(last)) {
          val (headEntry, _) = head
          println(s"($cnt) found $last")
          val (laatEntry, lastCardinal) = last


        }
        findMissing(tail, missing, head :: seen, head)
    }
  }

  override def rawSolution: List[GridEntry[Char]] = {
    val path = findPath

    val first = path.find {
      case (entry, _) => entry.equalPosition(53, 128)
    }
    println(first)

    val missing: List[PathEntry] = DecSixDiffer.missing
 /*   missing.foreach {
      case (entry, cardinal) =>
        println(s"Pos(${entry.xPosition},${entry.yPosition}),$cardinal")
    }
 */
    //findMissing(path.zipWithIndex, missing)
    List.empty
  }
}
