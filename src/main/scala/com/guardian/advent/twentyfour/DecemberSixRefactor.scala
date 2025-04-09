package com.guardian.advent.twentyfour

import com.guardian.advent.{Cardinal, GridEntry}

trait DecemberSixRefactor extends December[Int, CharGrid, GridEntry[Char]] with DecemberSixParser {

  type PathEntry = (GridEntry[Char], Cardinal)

  override def day: Int = 6
  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  val grid = rawInput
  val begin = grid.entries.collectFirst{
    case start: Start => start
  }

  protected def verticeToBlock(start: GridEntry[Char], cardinal: Cardinal): List[PathEntry] = {
    grid.vertice(start, cardinal) { case (entry, entries) =>
      (entry :: entries).collectFirst {
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
    maybeVerticeStart.map{
      case(entry, cardinal) =>
        val vertice = verticeToBlock(entry, cardinal)
        val maybeNextVerticeStart = vertice.headOption.flatMap {
          case (head, cardinal) => nextVerticeStart(head, cardinal.nextCardinal)
        }
        walkPath(maybeNextVerticeStart, vertice ::: pathSoFar)
    }.getOrElse(pathSoFar.reverse)
  }

  protected def findPath: List[GridEntry[Char]] = begin.map { start =>
    val firstStart = grid.nextEntryByDirection(start, start.cardinal).map { entry => (entry, start.cardinal) }
    walkPath(firstStart, List((start, start.cardinal)))
      .map{ case (entry, _) => entry}
      .toSet
      .toList
   }.getOrElse(List.empty)
}

trait DecemberSixRefactorPartOne extends DecemberSixRefactor {
  override def rawSolution: List[GridEntry[Char]] = findPath
}

object DecemberSixRefactorPartOneTest extends DecemberSixRefactorPartOne with PuzzleTest
object DecemberSixRefactorPartOneSolution extends DecemberSixRefactorPartOne with PuzzleSolution

trait DecemberSixRefactorPartTwoTest extends DecemberSixRefactor with PuzzleSolution {

  type BlockMap = Map[GridEntry[Char], List[Cardinal]]

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

  private def rawVerticeToblock(verticeStart: GridEntry[Char], cardinal: Cardinal): List[GridEntry[Char]] = {
    verticeToBlock(verticeStart, cardinal).map{ case(entry, _) => entry }
  }

  //TODO move blokmap methods to class
  private def updateBlockMapForVertice(vertice: List[GridEntry[Char]], cardinal: Cardinal, blockMap: BlockMap): BlockMap = {

    val headForVertice = blockForVertice(vertice, cardinal)

    headForVertice.map {
      head => getBlocksForVertice(head, cardinal).foldRight(blockMap) {
        case(pathEntry, blockMap) => updateBlockMap(blockMap, pathEntry)
      }
    }.getOrElse(blockMap)
  }

  //Vertice class
  private def blockForVertice(vertice: List[GridEntry[Char]], cardinal: Cardinal): Option[GridEntry[Char]] = {
     vertice.headOption.flatMap{ verticeHead => grid.nextEntryByDirection(verticeHead, cardinal)}
  }


  def getBlocksForVertice(lastHead: GridEntry[Char], cardinal: Cardinal, blocksEntries: List[PathEntry] = List.empty):  List[PathEntry] = {
    grid.nextEntryByDirection(lastHead, cardinal) match {
      case Some(block: Block) => getBlocksForVertice(lastHead, cardinal.nextCardinal, (block, cardinal) :: blocksEntries)
      case None => blocksEntries
    }
  }

  //Not Path Entry
  private def checkLoop(maybeVerticeStart: Option[PathEntry], blockCandidate: GridEntry[Char], blockMap: BlockMap): Boolean = {

    def blockHitPreviously(blockEntry: GridEntry[Char], cardinal: Cardinal): Boolean = {
       blockMap.get(blockEntry).map{
         cadinalsForBlock => cadinalsForBlock.contains(cardinal) }
       .getOrElse(false)
    }

    def checkBlock(vertice: List[GridEntry[Char]], cardinal: Cardinal): Boolean = {
      val maybeBlock = vertice.contains(blockCandidate) match {
        case true => vertice.reverse
          .takeWhile{ entry => entry != blockCandidate }
          .lastOption
        case false => blockForVertice(vertice, cardinal)
      }

      maybeBlock.map{ block => blockHitPreviously(block, cardinal)}.getOrElse(false)
    }

    maybeVerticeStart.map {
      verticeStart =>

        val (startEntry, cardinal) = verticeStart
        val vertice = rawVerticeToblock(startEntry, cardinal)

        vertice match {
          case Nil => false
          case _ :: _ =>
            val isLoop = checkBlock(vertice, cardinal)
            isLoop || {
              val nextBlockMap = updateBlockMapForVertice(vertice, cardinal, blockMap)
              val maybeNextVerticeStart = vertice.headOption.flatMap{
                head => nextVerticeStart(head, cardinal.nextCardinal)
              }
              checkLoop(maybeNextVerticeStart, blockCandidate, nextBlockMap)
            }
        }
    }.getOrElse(false)
  }



  def checkVertice(vertice: List[PathEntry], visited: List[PathEntry], blockMap: BlockMap, blocks: List[GridEntry[Char]]): List[GridEntry[Char]] = {

    def isBlockCandidate(entry: GridEntry[Char], visitedEntries: List[GridEntry[Char]]): Boolean = {
      !entry.isInstanceOf[Start] && visitedEntries.doesNotContain(entry)
    }

    vertice match {
      case head :: next :: tail =>
        val (maybeBlock, cardinal) = next
        lazy val loopStart = (head._1, cardinal.nextCardinal)
        val visitedEntries = visited.map{ case(entry,  _) => entry }
        val isThisLoop = isBlockCandidate(maybeBlock, visitedEntries) && checkLoop(Some(loopStart), maybeBlock, blockMap)
        val nextBlocks = if(isThisLoop) maybeBlock :: blocks else blocks
        checkVertice(next :: tail, head :: visited, blockMap, nextBlocks)
      case _ => blocks
    }
  }

  def findBlocks( pathRemaining: List[PathEntry], visited: List[PathEntry] = List.empty, blockMap: BlockMap = Map.empty, blocks: List[GridEntry[Char]] = List.empty ): List[GridEntry[Char]] = {
     pathRemaining match {
       case Nil => blocks
       case _ =>
          val (currentVertice, nextPathRemaining) = getCurrentVertice(pathRemaining)
         // val currentLoops = checkVertice(currentVertice,

     }
     List.empty


  }

  override def rawSolution: List[GridEntry[Char]] = {
     val path = findPath
    List.empty
  }
}