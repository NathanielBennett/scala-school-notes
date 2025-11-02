package com.guardian.advent.twentyfour


import com.guardian.advent.{AdventOfCodeGridParser, AdventOfCodeParser, GridComboParser}
import com.guardian.advent.grid.{Block, Cardinal, CharGrid, East, GridEntry, MovableBlock, North, South, Space, West}
import com.guardian.advent.twentyfour.DecemberFifteenPartTwoTest.expandGrid

import scala.util.Try

trait DecemberFifteenParser extends GridComboParser[Char, CharGrid, List[Cardinal], List[List[Cardinal]]] {

  class RobotWarehouseGridParser(override val day: Int, override val test: Boolean) extends AdventOfCodeGridParser[Char, CharGrid] {

    override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = Try {
      value match {
        case '#' => Block(x, y, value)
        case 'O' => MovableBlock(x, y, value)
        case '.' => Space(x, y, value)
        case '@' => Space(x, y, value)
      }
    }.toOption

    override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
  }
  class CardinalParser(override val day: Int, override val test: Boolean) extends AdventOfCodeParser[List[Cardinal], List[List[Cardinal]]] {

    override def lineParser(line: String): Option[List[Cardinal]] = Option(line.toList.flatMap { case ch => Cardinal(ch) })

    override def sequenceToCollection(seq: Seq[List[Cardinal]]): List[List[Cardinal]] = seq.toList
  }

  override def instructionParser: AdventOfCodeParser[List[Cardinal], List[List[Cardinal]]] = new CardinalParser(day, test)
  override def gridParser: AdventOfCodeGridParser[Char, CharGrid] = new RobotWarehouseGridParser(day, test)
}

trait DecemberFifteen extends December[Int, (CharGrid, List[List[Cardinal]]), Int] with DecemberFifteenParser {

  override def day: Int = 15

  override def solver: Solver[Int, Int] = listTotalSolver(0, test)

  val (startGrid, rawCardinals) = rawInput
  lazy val moveList = rawCardinals.flatten

  protected def verticeToEdge(start: GridEntry[Char], cardinal: Cardinal, grid: CharGrid): List[GridEntry[Char]] = {
    grid.vertice(start, cardinal) {
        case (entry, _) => Option(entry).collectFirst {
          case block: Block => block
        }.isDefined
      }
      .reverse
  }

  def updateGrid(grid: CharGrid, moved: Set[GridEntry[Char]]): CharGrid = {
    val newEntries = grid.entries.filterNot {
      case entry =>
        moved.exists(movedEntry => movedEntry.equalPosition(entry.xPosition, entry.yPosition))
    } ++ moved
    grid.update(newEntries)
  }

  def shiftBlocks(verticeTail: List[GridEntry[Char]], robot: GridEntry[Char], cardinal: Cardinal, blocks: List[GridEntry[Char]] = List.empty, maybeSpace: Option[GridEntry[Char]] = None): (GridEntry[Char], Set[GridEntry[Char]]) = {

    lazy val nextRobot: GridEntry[Char] = {
      val (x, y) = robot.nextCoords(cardinal)
      Space(x, y, robot.value)
    }

    if (maybeSpace.isDefined) {
      val newSpace = Space(robot.xPosition, robot.yPosition, '.')
      val movedBlocks = blocks.map { block =>
        val (x, y) = block.nextCoords(cardinal)
        MovableBlock(x, y, block.value)
      }.reverse
      (nextRobot, (newSpace :: nextRobot :: movedBlocks ::: verticeTail).reverse.toSet)
    }
    else verticeTail match {
      case Nil => (robot, Set.empty)
      case head :: tail =>
        if (head.isInstanceOf[MovableBlock]) shiftBlocks(tail, robot, cardinal, head :: blocks)
        else shiftBlocks(tail, robot, cardinal, blocks, Some(head))
    }
  }

  def updateVertex(robot: GridEntry[Char], vertice: List[GridEntry[Char]], cardinal: Cardinal): (GridEntry[Char], Set[GridEntry[Char]]) = {
    vertice match {
      case Nil => (robot, Set.empty)
      case head :: _ =>
        head match {
          case space: Space =>
            val nextRobot = space.copy(value = robot.value)
            (nextRobot, Set(nextRobot, Space(robot.xPosition, robot.yPosition, '.')))
          case _ => shiftBlocks(vertice, robot, cardinal)
        }
    }
  }

 def transformGrid(grid: CharGrid, robot: GridEntry[Char], cardinal: Cardinal): Option[(GridEntry[Char], CharGrid)] = {
    val vertice = verticeToEdge(robot, cardinal, grid)

    vertice.headOption.map { robotOption =>
      val (r, shiftedBlocks) = updateVertex(robotOption, vertice.tail, cardinal)
      (r, updateGrid(grid, shiftedBlocks))
    }
  }

  def moveRobotFold(moveList: List[Cardinal], grid: CharGrid, robot: GridEntry[Char], mv: Int = 1) : CharGrid = {
    val (_, endGrid) = moveList.zipWithIndex.foldLeft((robot, grid)) {
      case (gridAndRobot, (cardinal, cnt)) =>
        val (thisRobot, thisGrid) = gridAndRobot
        transformGrid(thisGrid, thisRobot, cardinal)
          .getOrElse((thisRobot, thisGrid))
    }
    endGrid
  }

  def getCrates(grid: CharGrid): List[GridEntry[Char]]

  override def rawSolution: List[Int] = {

    startGrid.findEntry('@').map {
      robot =>
        val endGrid = moveRobotFold(moveList, startGrid, robot)
        getCrates(endGrid)
          .map {
            case gridEntry => gridEntry.yPosition * 100 + gridEntry.xPosition
          }
    }.getOrElse(List.empty)
  }
}

 trait DecemberFifteenPartOne extends DecemberFifteen {
   override def getCrates(grid: CharGrid): List[GridEntry[Char]]  = {
     def isCrate(gridEntry: GridEntry[Char]): Boolean = Option(gridEntry).collect { case movableBlock: MovableBlock => movableBlock }.isDefined

     grid.filterEntries(isCrate)
   }

}

object DecemberFifteenPartOneTest extends DecemberFifteenPartOne with PuzzleTest
object DecemberFifteenPartOneSolution extends DecemberFifteenPartOne with PuzzleSolution

trait DecemberFifteenPartTwo extends DecemberFifteen {

  def nextRowCalculator(cardinal: Cardinal) : Int => Int = {
    if(cardinal == South) { (row: Int) => row + 1 }
    else (row: Int) => row - 1
  }

  def moveBlockInt(gridEntry: GridEntry[Char], nextRowIndex: Int): GridEntry[Char] = {
    def shift: PartialFunction[GridEntry[Char], GridEntry[Char]]  = {
      case space: Space => Space(space.xPosition, nextRowIndex, space.value )
      case movableBlock: MovableBlock => MovableBlock(movableBlock.xPosition, nextRowIndex, movableBlock.value)
    }
    shift(gridEntry)
  }

  def getInitialBlock(block: GridEntry[Char], nextFullRow: List[GridEntry[Char]]): List[GridEntry[Char]] = {
    val (min, max) = (block.xPosition - 1, block.xPosition + 1)
    nextFullRow.filter { entry => entry.xPosition >= min && entry.xPosition <= max } match {
      case Nil => List.empty
      case list  => if (list.head.value == '[') list.take(2) else list.tail
    }
  }

  def getNextRow(blocks: List[GridEntry[Char]], grid: CharGrid, nextRowIndex: Int): List[GridEntry[Char]] = {

    lazy val nextFullRow = grid.filterEntries( entry => entry.yPosition == nextRowIndex).sortBy(_.xPosition)

//    println(s"Next row: ${nextRowIndex}")
    blocks match {
      case Nil => List.empty
      case head :: Nil => getInitialBlock(head, nextFullRow)
      case _ :: _ =>
        val (min, max) = (blocks.minBy(_.xPosition).xPosition,  blocks.maxBy(_.xPosition).xPosition)
        val matchingBLocks = nextFullRow.sliceTo(min, max)
        matchingBLocks match {
          case Nil => matchingBLocks
          case head :: tail =>
            val toLeft = if (head.value == ']') nextFullRow(min - 1) :: matchingBLocks else matchingBLocks
            tail.lastOption.map{
              last => if(last.value == '[') toLeft.appended(nextFullRow(max + 1)) else toLeft
            }.getOrElse(toLeft)
        }
    }
  }

  def getAllBlocksToShift(currRowEntries: List[GridEntry[Char]], currRowIndex: Int,  nextRowIndexCalculator: Int => Int, grid: CharGrid, acc: List[GridEntry[Char]] = List.empty): List[GridEntry[Char]] = {

    def spaceForBlock(block: (GridEntry[Char], GridEntry[Char]), row: List[GridEntry[Char]]): Option[(GridEntry[Char], GridEntry[Char])] = {
      val (leftBlock, rightBlock) = block
      for {
        left <- row.find { entry => entry.xPosition == leftBlock.xPosition }
        right <- row.find { entry => entry.xPosition == rightBlock.xPosition }
        _ <- Option(left).collectFirst { case space: Space => space }
        _ <- Option(right).collectFirst { case space: Space => space }
      } yield (left, right)
    }

    def pairBlocks(row: List[GridEntry[Char]], acc: List[(GridEntry[Char], GridEntry[Char])] = List.empty, maybeHalfBlock: Option[MovableBlock] = None): List[(GridEntry[Char], GridEntry[Char])] = {
      row match {
        case Nil => acc.reverse
        case head :: tail =>
          val maybeHeadBlock = Option(head).collectFirst { case block: MovableBlock => block }
          (maybeHeadBlock, maybeHalfBlock) match {
            case (Some(headBlock), Some(halfBlock)) if (halfBlock.value == '[' && halfBlock.value == ']') =>
              pairBlocks(tail, (halfBlock, headBlock) :: acc, None)
            case (Some(headBlock), _) if (headBlock.value == '[') =>
              pairBlocks(tail, acc, Some(headBlock))
            case _ => pairBlocks(tail, acc, None)
          }
      }
    }

    def topBlocks(currentRow: List[GridEntry[Char]], targetRow: List[GridEntry[Char]]): List[GridEntry[Char]] = {

      def loop(blockPairs: List[(GridEntry[Char], GridEntry[Char])], acc: List[(GridEntry[Char], GridEntry[Char])] = List.empty): List[GridEntry[Char]] = {
        blockPairs match {
          case Nil => acc.flattenList
          case head :: tail =>
            spaceForBlock(head, targetRow) match {
              case Some(spaces) => loop(tail, spaces :: acc)
              case None => List.empty
            }
        }
      }

      val pairs = pairBlocks(currentRow)
      loop(pairs)
    }

    def updateRowForMoveBlocks(blocks: List[GridEntry[Char]]) : List[GridEntry[Char]]  = {
      blocks.map { block =>
        val updatedYPosition = nextRowIndexCalculator(block.yPosition)
        moveBlockInt(block, updatedYPosition)
      }
    }

    val nextRowIndex = nextRowIndexCalculator(currRowIndex)
    val blocksForCurrentRow = currRowEntries.find{ entry => entry.value == '@' }
      .map{ robot => List(robot) }
      .getOrElse { currRowEntries.collect{ case movableBlock: MovableBlock => movableBlock } }

    val nextRow = getNextRow(blocksForCurrentRow, grid, nextRowIndex)
    val nextRowKeys = nextRow.groupBy { entry => entry.getClass.getSimpleName }.keys
    nextRowKeys.toList match {
      case l if l.contains("Block") => List.empty
      case List("Space") => acc ++ currRowEntries
      case _ =>
        topBlocks(currRowEntries, nextRow) match {
          case Nil => getAllBlocksToShift(nextRow, nextRowIndex, nextRowIndexCalculator, grid, acc ++ currRowEntries)
          case blocks => updateRowForMoveBlocks(blocks ::: acc)
        }
    }
  }

  def getShiftedBlocks(blocksToShift: List[GridEntry[Char]], cardinal: Cardinal): Set[GridEntry[Char]] = {

    val rowCalculator = nextRowCalculator(cardinal)

    def loop(blocksByRow: List[(Int, List[GridEntry[Char]])], lastRow: List[GridEntry[Char]] = List.empty, acc: List[GridEntry[Char]] = List.empty): Set[GridEntry[Char]] = {
      blocksByRow match {
        case Nil => acc.toSet
        case head :: tail =>
          val (row, blocks) = head
          val nextRow = rowCalculator(row)
          val transformed = if (lastRow.isEmpty) blocks.map { bl => moveBlockInt(bl, nextRow) }
          else lastRow.filterNot { lastRowBlock => blocks.find{ bl => lastRowBlock.equalX(bl) }.isDefined }
            .map{ lastRowBlock => Space(lastRowBlock, '.') } ++ blocks.map{ bl => moveBlockInt(bl, nextRow) }
          val moved = if(tail.isEmpty) blocks.map{ bl => Space(bl, '.') } ++ transformed else transformed
          loop(tail, blocks, acc ::: moved)
        }
      }

      def blocksOrderedByRow: List[(Int, List[GridEntry[Char]])] = {
        val rawOrdered = blocksToShift.groupBy { entry => entry.yPosition }
          .toList
          .sortBy { case (row, _) => row }

        if (cardinal == North) rawOrdered else rawOrdered.reverse
      }
      loop(blocksOrderedByRow)
  }

  override def transformGrid(grid: CharGrid, robot: GridEntry[Char], cardinal: Cardinal): Option[(GridEntry[Char], CharGrid)] = {

    def moveBlock(gridEntry: GridEntry[Char], nextRowIndex: Int => Int): GridEntry[Char] = {
       val updatedRow = nextRowIndex(gridEntry.yPosition)
       def shift: PartialFunction[GridEntry[Char], GridEntry[Char]]  = {
          case space: Space => Space(space.xPosition, updatedRow, space.value )
          case movableBlock: MovableBlock => MovableBlock(movableBlock.xPosition, updatedRow, movableBlock.value)
        }
      shift(gridEntry)
    }

    if(!cardinal.isPolar) super.transformGrid( grid, robot, cardinal )
    else {

      val blockRowCalculator = nextRowCalculator(cardinal)

      grid.nextEntryByDirection(robot, cardinal).map { entry =>
        val shiftedBlocks = entry match {
          case space: Space =>
            val nextRobot = Space(space, robot.value)
            Set[GridEntry[Char]](nextRobot, Space(robot, '.'))
          case _: MovableBlock =>
            val shiftedBlocks = getAllBlocksToShift(List(robot), robot.yPosition, blockRowCalculator, grid)
            val shifted = getShiftedBlocks(shiftedBlocks, cardinal)
              shifted
           case _ => Set[GridEntry[Char]]()
        }
        if(shiftedBlocks.isEmpty) (robot, grid)
        else (
          moveBlock(robot, blockRowCalculator),
          updateGrid(grid, shiftedBlocks)
        )
      }
    }
  }

  private def entryToList(entry: GridEntry[Char]): Option[List[GridEntry[Char]]] = {
    Try {
      entry match {
        case block: Block => List(block.copy(xPosition = entry.xPosition * 2), block.copy(xPosition = entry.xPosition * 2 + 1))
        case movableBlock: MovableBlock => List(movableBlock.copy(xPosition = entry.xPosition * 2, value = '['), movableBlock.copy(xPosition = entry.xPosition * 2 + 1, value = ']'))
        case space: Space =>
          if (space.value == '@') List(space.copy(xPosition = entry.xPosition * 2), space.copy(xPosition = entry.xPosition * 2 + 1, value = '.'))
          else  List(space.copy(xPosition = entry.xPosition * 2), space.copy(xPosition = entry.xPosition * 2 + 1))
      }
    }.toOption
  }


  protected def expandGrid(grid: CharGrid): CharGrid = {
    val transformedEntries = grid.entries.flatMap{ entry => entryToList(entry) }.flatten
    CharGrid(transformedEntries)
  }

  override val (startGrid, rawCardinals) = {
    val (rawGrid, rawCardinals) = rawInput
    (expandGrid(rawGrid), rawCardinals)
  }

  override def getCrates(grid: CharGrid): List[GridEntry[Char]] = grid.filterEntries {ge => ge.value == '[' }
}

object DecemberFifteenPartTwoTest extends DecemberFifteenPartTwo with PuzzleTest// with App
object DecemberFifteenPartTwoSolution extends DecemberFifteenPartTwo with PuzzleSolution