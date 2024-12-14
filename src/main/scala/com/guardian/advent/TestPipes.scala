package com.guardian.advent

import scala.io.Source
import scala.util.Try

sealed trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction


object TestPipes extends App {


  sealed trait Tile {

    def xPos: Int

    def yPos: Int

    def pipe: Char

    def vertex: Boolean = false

    def startTiles(tiles: Array[Array[Tile]]): (Tile, List[(Tile, Direction)]) = {
      val directions = neighbours(tiles).flatMap {
        neighbour => neighbour.startTileDirection(this).map { direction => (neighbour, direction) }
      }

      val normalizedStart = Try {
        directions.map { case (_, direction) => direction } match {
          case List(North, South) | List(South, North) => VerticalPipe(xPos, yPos)
          case List(South, East) | List(East, South) => NorthToEastPipe(xPos, yPos)
          case List(West, South) | List(South,West) => NorthToWestPipe(xPos, yPos)
        }
      }.toOption
      normalizedStart.map { st => tiles(yPos)(xPos) = st }
      (normalizedStart.getOrElse(this), directions)
    }

    def neighbours(tiles: Array[Array[Tile]]): List[Tile] = {
      val coOrds = List((xPos, yPos - 1), (xPos, yPos + 1), (xPos - 1, yPos), (xPos + 1, yPos))
        .filter { case (x, y) => x >= 0 && y >= 0 && x < tiles.head.length && y < tiles.length }
      coOrds.map { case (x, y) => tiles(y)(x) }
    }

    def nextTile(tiles: Array[Array[Tile]], direction: Direction): (Tile, Direction) = {
      val (nextX, nextY, nextDirection) = nextPosition(direction)
      (tiles(nextY)(nextX), nextDirection)
    }

    protected def nextPosition(direction: Direction): (Int, Int, Direction) = (0, 0, direction)

    protected def startTileDirection(startTile: Tile): Option[Direction] = None

    def isNorthOf(otherTile: Tile): Boolean = otherTile.xPos == xPos && otherTile.yPos < yPos

    def isSouthOf(otherTile: Tile): Boolean = otherTile.xPos == xPos && otherTile.yPos > yPos

    def isEastOf(otherTile: Tile): Boolean = otherTile.xPos < xPos && otherTile.yPos == yPos

    def isWestOf(otherTile: Tile): Boolean = otherTile.xPos > xPos && otherTile.yPos == yPos

    def isEnclosed(allTiles: List[Tile], vertices: List[Tile]): Boolean = {
      def filterRange(range: List[Tile], crossedVertices: List[Tile] = List.empty): Boolean = range match {
        case Nil => crossedVertices.size % 2 == 1
        case head :: tail =>
          val newCrossedVertices = vertices.contains(head) match {
            case true => head :: crossedVertices
            case false => crossedVertices
          }
          filterRange(tail, newCrossedVertices)
      }

      val rangeLeft = allTiles.filter { t => t.yPos == yPos && t.xPos < xPos }
      val rangeRight = allTiles.filter { t => t.yPos == yPos && t.xPos > xPos }
      filterRange(rangeLeft) && filterRange(rangeRight)
    }

    def isEnclosedL(allTiles: List[Tile], vertices: List[Tile]): List[Tile] = {
      def filterRange(range: List[Tile], crossedVertices: List[Tile] = List.empty): List[Tile] = range match {
        case Nil => crossedVertices
        case head :: tail =>
          val newCrossedVertices = vertices.contains(head) match {
            case true => head :: crossedVertices
            case false => crossedVertices
          }
          filterRange(tail, newCrossedVertices)
      }

      filterRange(allTiles.filter { t => t.yPos == yPos && t.xPos < xPos })
    }

    def isEnclosedRip(allTiles: List[Tile], loop: List[Tile], northVertices: List[Tile], southVertices: List[Tile]): Boolean = {
      def filterRange(range: List[Tile], vertices: List[Tile], crossedVertices: List[Tile] = List.empty): Boolean = range match {
        case Nil => crossedVertices.size % 2 == 1
        case head :: tail =>
          val newCrossedVertices = vertices.contains(head) match {
            case true => head :: crossedVertices
            case false => crossedVertices
          }
          filterRange(tail, vertices, newCrossedVertices)
      }

      lazy val range = allTiles.filter(tile => yPos == tile.yPos)
      lazy val oddSouth = filterRange(range, southVertices)
      lazy val oddNorth = filterRange(range, northVertices)
      !loop.contains(this) && oddNorth && oddSouth
    }

  }


  object Tile {
    def apply(xPos: Int, yPos: Int, pipe: Char): Tile = pipe match {
      case 'S' => Start(xPos, yPos)
      case '-' => HorizontalPipe(xPos, yPos)
      case '|' => VerticalPipe(xPos, yPos)
      case 'L' => SouthToEastPipe(xPos, yPos)
      case 'J' => SouthToWestPipe(xPos, yPos)
      case 'F' => NorthToEastPipe(xPos, yPos)
      case '7' => NorthToWestPipe(xPos, yPos)
      case _ => Ground(xPos, yPos)
    }
  }

  case class Start(override val xPos: Int, override val yPos: Int) extends Tile {
    override val pipe = 'S'
  }

  case class HorizontalPipe(override val xPos: Int, override val yPos: Int) extends Tile {
    override val pipe: Char = '-'

    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isWestOf(startTile), isEastOf(startTile)) match {
        case (true, false) => Some(West)
        case (false, true) => Some(East)
        case _ => None
      }

    override def nextPosition(direction: Direction): (Int, Int, Direction) = direction match {
      case East => (xPos + 1, yPos, direction)
      case West => (xPos - 1, yPos, direction)
    }
  }

  case class VerticalPipe(override val xPos: Int, override val yPos: Int, override val vertex: Boolean = true) extends Tile {
    override val pipe: Char = '|'

    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isNorthOf(startTile), isSouthOf(startTile)) match {
        case (true, false) => Some(South)
        case (false, true) => Some(North)
        case _ => None
      }

    override def nextPosition(direction: Direction): (Int, Int, Direction) = direction match {
      case North => (xPos, yPos - 1, direction)
      case South => (xPos, yPos + 1, direction)
    }
  }

  case class SouthToEastPipe(override val xPos: Int, override val yPos: Int) extends Tile {
    override val pipe: Char = 'L'


    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isNorthOf(startTile), isWestOf(startTile)) match {
        case (true, false) => Some(South)
        case (false, true) => Some(East)
        case _ => None
      }

    override protected def nextPosition(direction: Direction) = direction match {
      case South => (xPos + 1, yPos, East)
      case West => (xPos, yPos - 1, North)
    }
  }

  case class SouthToWestPipe(override val xPos: Int, override val yPos: Int) extends Tile {
    override val pipe: Char = 'J'

    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isNorthOf(startTile), isEastOf(startTile)) match {
        case (true, false) => Some(South)
        case (false, true) => Some(East)
        case _ => None
      }

    override protected def nextPosition(direction: Direction) = direction match {
      case South => (xPos - 1, yPos, West)
      case East => (xPos, yPos - 1, North)
    }
  }

  case class NorthToEastPipe(override val xPos: Int, override val yPos: Int, override val vertex: Boolean = true) extends Tile {
    override val pipe: Char = 'F'

    override protected def nextPosition(direction: Direction) = direction match {
      case North => (xPos + 1, yPos, East)
      case West => (xPos, yPos + 1, South)
    }

    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isWestOf(startTile), isSouthOf(startTile)) match {
        case (true, false) => Some(South)
        case (false, true) => Some(East)
        case _ => None
      }
  }

  case class NorthToWestPipe(override val xPos: Int, override val yPos: Int, override val vertex: Boolean = true) extends Tile {
    override val pipe: Char = '7'

    override protected def startTileDirection(startTile: Tile): Option[Direction] =
      (isEastOf(startTile), isNorthOf(startTile)) match {
        case (true, false) => Some(East)
        case (false, true) => Some(North)
        case _ => None
      }

    override protected def nextPosition(direction: Direction) = direction match {
      case North => (xPos - 1, yPos, West)
      case East => (xPos, yPos + 1, South)
    }
  }

  case class Ground(override val xPos: Int, override val yPos: Int) extends Tile {
    override val pipe: Char = '.'
  }


  def findStart(tiles: Array[Array[Tile]]): Option[Tile] = {
    def loopRows(row: Array[Array[Tile]]): Option[Tile] = {
      Option(row.head).flatMap { r => r.find(t => t.pipe == 'S') } match {
        case Some(start) => Some(start)
        case None => loopRows(row.tail)
      }
    }

    loopRows(tiles)
  }


  def getLoop(tiles: Array[Array[Tile]], start: Tile, firstTile: Tile, firstDirection: Direction): List[Tile] = {
    def traverseLoop(tile: Tile, direction: Direction, seen: List[Tile] = List(start)): List[Tile] = {
      if (tile == start) {
        seen
      }
      else {
        val (nextTile, nextDirection) = tile.nextTile(tiles, direction)
        traverseLoop(nextTile, nextDirection, tile :: seen)
      }
    }

    traverseLoop(firstTile, firstDirection)
  }


  val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day10.txt").getLines().toList
  val rows = lines.length
  val cols = lines.head.length
  val tiles = Array.ofDim[Tile](rows, cols)

  lines.zipWithIndex.foreach {
    case (line, yPosition) =>
      line.zipWithIndex.foreach { case (pipe, xPosition) =>
        tiles(yPosition)(xPosition) = Tile(xPosition, yPosition, pipe)
      }
  }

  val northVerticePipes = "F7|"
  val southVerticePipes = "LJ|"

  val t = findStart(tiles).map {
    start =>
      val (actualStart, firstTiles) = start.startTiles(tiles)
      println(s"Actual: $actualStart: ${actualStart.pipe} ${firstTiles}")
      val (firstTile, firstDirection) = firstTiles.head
      val allTiles = tiles.flatMap { t => t.toList }.toList
      val loopTiles = getLoop(tiles, actualStart, firstTile, firstDirection)
      val nonLoopTiles = allTiles.filterNot { t => loopTiles.contains(t) }
      // println(L"")
      val northVertices = loopTiles.filter(t => northVerticePipes.contains(t.pipe))
      val southVertices = loopTiles.filter { t => southVerticePipes.contains(t.pipe) }

      // nonLoopTiles.filter{ t => t.isEnclosedRip(allTiles, loopTiles, northVertices, southVertices) }.size
      nonLoopTiles.filter { t => t.isEnclosed(allTiles, northVertices) }.size

  }
}
