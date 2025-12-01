package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, December}
import com.guardian.advent.grid.{Block, Cardinal, CharGrid, East, EmptyEntry, GridEntry, North, South, Space, Start, West}

import scala.util.Try

trait DecemberSixParser extends AdventOfCodeGridParser[Char, CharGrid] {

  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = Try { value match {
    case '.' => Space(x, y, value)
    case '>' => Start(x, y, value, East)
    case '<' => Start(x, y, value, West)
    case 'v' => Start(x, y, value, South)
    case '^' => Start(x, y, value, North)
    case '#' => Block(x, y, value)
  }}.toOption

  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}


trait DecemberSix extends December[Int, CharGrid, GridEntry[Char]] with DecemberSixParser {

  type PathEntry = (GridEntry[Char], Cardinal)
  type BlockMap = Map[GridEntry[Char], List[Cardinal]]

  override def day: Int = 6

  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  val grid = rawInput
  val begin: Option[PathEntry] = grid.entries.collectFirst {
    case start: Start => (start, start.cardinal)
  }

  protected def verticeToBlock(start: GridEntry[Char], cardinal: Cardinal, blockMap: BlockMap = Map.empty): List[PathEntry] = {
    grid.vertice(start, cardinal) { case (entry, entries) =>
        blockMap.keySet.contains(entry) || (entry :: entries).collectFirst {
          case block: Block => block
        }.isDefined
      }
      .map { entry => (entry, cardinal) }
  }

  protected def nextVerticeStart(gridEntry: GridEntry[Char], cardinal: Cardinal): Option[PathEntry] = {
    if (grid.isEdge(gridEntry)) None
    else {
      val maybeSpace = grid.nextEntryByDirection(gridEntry, cardinal).collectFirst {
        case emptyEntry: EmptyEntry => emptyEntry
      }
      maybeSpace.map { _ => (gridEntry, cardinal) }.orElse {
        nextVerticeStart(gridEntry, cardinal.nextCardinal)
      }
    }
  }

  protected def walkPath(maybeVerticeStart: Option[PathEntry], pathSoFar: List[PathEntry] = List.empty): List[PathEntry] = {

    maybeVerticeStart.map {
      case (entry, cardinal) =>
        val vertice = verticeToBlock(entry, cardinal)
        val maybeNextVerticeStart = vertice.headOption.flatMap {
          case (head, cardinal) => nextVerticeStart(head, cardinal.nextCardinal)
        }
        walkPath(maybeNextVerticeStart, vertice ::: pathSoFar)
    }.getOrElse(pathSoFar.reverse)
  }
}

trait DecemberSixPartOne extends DecemberSix {

  override def rawSolution: List[GridEntry[Char]] = {
    walkPath(begin).map { case(entry, _) => entry}
      .toSet
      .toList
  }
}

object DecemberSixPartOneTest extends DecemberSixPartOne with PuzzleTest
object DecemberSixPartOneSolution extends DecemberSixPartOne with PuzzleSolution

trait DecemberSixPartTwo extends DecemberSix {

  implicit def pathToVertice(path: List[PathEntry]): List[GridEntry[Char]] = path.map{ case(entry, _) => entry}

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

  private def  updateBlockMapForVertice(head: PathEntry, blockMap: BlockMap): BlockMap = {
     val (headEntry, cardinal) = head

     getBlocksForVertice(headEntry, cardinal).foldRight(blockMap) {
          case(pathEntry, blockMap) => updateBlockMap(blockMap, pathEntry)
      }
  }

  private def blockForVertice(vertice: List[GridEntry[Char]], cardinal: Cardinal): Option[GridEntry[Char]] = {
     vertice.headOption.flatMap{ verticeHead => grid.nextEntryByDirection(verticeHead, cardinal)}
  }

  private def getBlocksForVertice(lastHead: GridEntry[Char], cardinal: Cardinal, blocksEntries: List[PathEntry] = List.empty):  List[PathEntry] = {
    grid.nextEntryByDirection(lastHead, cardinal) match {
      case Some(block: Block) => getBlocksForVertice(lastHead, cardinal.nextCardinal, (block, cardinal) :: blocksEntries)
      case _ => blocksEntries
    }
  }

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

    def isBlockCandidate(entry: GridEntry[Char]): Boolean =  visited.map{ case(entry, _) => entry}.doesNotContain(entry)

    vertice match {
      case head :: next :: tail =>
        val (headEntry, cardinal) = head
        lazy val loopStart = (headEntry, cardinal.nextCardinal)
        val isThisLoop = isBlockCandidate(next._1) && {
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
     pathRemaining match {
       case Nil => blocks
       case _ :: _ =>
         val (currentVertice, nextPathRemaining) = getCurrentVertice(pathRemaining)
         val blocksForVertice = checkVertice(currentVertice, visited, blockMap)
         val nextBlockMap =
           currentVertice.reverse.headOption.map{ head => updateBlockMapForVertice(head, blockMap) }.getOrElse(blockMap)
         findBlocks(nextPathRemaining, currentVertice.tail ::: visited, nextBlockMap, blocksForVertice ::: blocks)
     }
  }

  override def rawSolution: List[GridEntry[Char]] = {
     val path = walkPath(begin)
     findBlocks(path)
  }
}

object DecemberSixRefactorPartTwoTest extends DecemberSixPartTwo with PuzzleTest
object DecemberSixRefactorPartTwoSolution extends DecemberSixPartTwo with PuzzleSolution

