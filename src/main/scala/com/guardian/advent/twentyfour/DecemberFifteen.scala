package com.guardian.advent.twentyfour


import com.guardian.advent.{AdventOfCodeGridParser, AdventOfCodeParser, GridComboParser}
import com.guardian.advent.grid.{Block, Cardinal, CharGrid, GridEntry, MovableBlock, Space}

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
  val moveList = rawCardinals.flatten

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
            val mextRobot = space.copy(value = robot.value)
            (mextRobot, Set(mextRobot, Space(robot.xPosition, robot.yPosition, '.')))
          case _ => shiftBlocks(vertice, robot, cardinal)
        }
    }
  }

  def moveRobotFold(moveList: List[Cardinal], grid: CharGrid, robot: GridEntry[Char], mv: Int = 1) : CharGrid = {
    val (_, endGrid) = moveList.zipWithIndex.foldLeft((robot, grid)) {
      case (gridAndRobot, (cardinal, cnt)) =>
        val (thisRobot, thisGrid) = gridAndRobot
        val vertice = verticeToEdge(thisRobot, cardinal, thisGrid)
        vertice.headOption.map { robotOption =>
          println()
          println(s"$cnt: $cardinal")
          println(s"Old grid ${grid.size}")
          val (r, shiftedBlocks) = updateVertex(robotOption, vertice.tail, cardinal)
          val newGrid = updateGrid(grid, shiftedBlocks)
          newGrid.printGrid()
          println("----")
          println(s"New grid ${newGrid.size}")
          val oldRow = grid.filterEntries( e => e.yPosition == 3)
          val newRow = newGrid.filterEntries( e => e.yPosition == 3)
          (r, newGrid)
        }.getOrElse((thisRobot, thisGrid))
    }
    endGrid
  }
}

 trait DecemberFifteenPartOne extends DecemberFifteen {

  override def rawSolution: List[Int] = {

    def isCrate(gridEntry: GridEntry[Char]): Boolean = Option(gridEntry).collect { case movableBlock: MovableBlock => movableBlock }.isDefined

    startGrid.findEntry('@').map {
      robot =>
        moveRobotFold(moveList, startGrid, robot)
          .filterEntries(isCrate)
          .map {
            case gridEntry => gridEntry.yPosition * 100 + gridEntry.xPosition
          }
    }.getOrElse(List.empty)
  }
}

object DecemberFifteenPartOneTest extends DecemberFifteenPartOne with PuzzleTest
object DecemberFifteenPartOneSolution extends DecemberFifteenPartOne with PuzzleSolution

trait DecemberFifteenPartTwo extends DecemberFifteen {

  val orderedEntries = startGrid.sortedEntries
  val transformedEntries = orderedEntries.groupBy { case entry => entry.yPosition }

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

  private def transformGrid(grid: CharGrid): CharGrid = {
    val transformedEntries = grid.entries.flatMap{ entry => entryToList(entry) }.flatten
    CharGrid(transformedEntries)
  }


  val transformedGrid = transformGrid(startGrid)
  transformedGrid.printGrid()

  transformedGrid.findEntry('@').foreach {
    robot =>
      moveRobotFold(moveList.take(5), transformedGrid, robot)
  }
}

object DecemberFifteenPartTwoTest extends DecemberFifteenPartTwo with PuzzleTest with App {


  override def rawSolution: List[Int] = ???
}