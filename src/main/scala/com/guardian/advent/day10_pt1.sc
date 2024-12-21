
import scala.io.Source
sealed trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

sealed trait Tile {

  def xPos: Int

  def yPos: Int

  def pipe: Char

  def startTiles(tiles: Array[Array[Tile]]): List[(Tile, Direction)] =
    neighbours(tiles).flatMap{
      neighbour => neighbour.startTileDirection(this).map{ direction => (neighbour, direction) }
    }

  def neighbours(tiles: Array[Array[Tile]]): List[Tile] = {
    val coOrds = List((xPos, yPos - 1), (xPos, yPos + 1), (xPos - 1, yPos), (xPos + 1, yPos) )
      .filter{ case(x,y) => x >= 0 && y >= 0 && x < tiles.head.length && y < tiles.length }
    coOrds.map{ case(x,y) => tiles(y)(x) }
  }

  def nextTile(tiles: Array[Array[Tile]], direction: Direction): (Tile, Direction) = {
    val (nextX, nextY, nextDirection) = nextPosition(direction)
    //println(s"next($nextX, $nextY) $nextDirection  ")
    (tiles(nextY)(nextX), nextDirection)
  }

  protected def nextPosition(direction: Direction): (Int, Int, Direction) = (0,0, direction)
  protected def startTileDirection(startTile: Tile): Option[Direction] = None

  def isNorthOf(otherTile: Tile): Boolean = otherTile.xPos == xPos && otherTile.yPos < yPos
  def isSouthOf(otherTile: Tile): Boolean = otherTile.xPos == xPos && otherTile.yPos > yPos
  def isEastOf(otherTile: Tile): Boolean = otherTile.xPos < xPos && otherTile.yPos == yPos
  def isWestOf(otherTile: Tile): Boolean = otherTile.xPos > xPos && otherTile.yPos == yPos

  override def toString(): String = this match {
    case _: HorizontalPipe => s"HorizontalPipe($xPos, $yPos $pipe)"
    case _: VerticalPipe   => s"VerticalPipe($xPos, $yPos $pipe)"
    case _: NorthToWestPipe   => s"NorthToWestPipe($xPos, $yPos $pipe)"
    case _: NorthToEastPipe   => s"NorthToEastPipe($xPos, $yPos $pipe)"
    case _: SouthToEastPipe   => s"SouthToEastPipe($xPos, $yPos $pipe)"
    case _: SouthToWestPipe   => s"SouthToWestPipe($xPos, $yPos $pipe)"
    case _: Start   => s"StartPipe($xPos, $yPos $pipe)"
    case _: Ground   => s"Ground ($xPos, $yPos $pipe)"

  }
}

object Tile {
  def apply(xPos: Int, yPos: Int, pipe: Char) : Tile = pipe match {
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
      case(true, false) => Some(West)
      case(false, true) => Some(East)
      case _ => None
    }

  override def nextPosition(direction: Direction): (Int, Int, Direction) =  direction match {
    case East => (xPos + 1, yPos, direction)
    case West => (xPos - 1, yPos, direction)
  }
}

case class VerticalPipe(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = '|'

  override protected def startTileDirection(startTile: Tile): Option[Direction] =
    (isNorthOf(startTile), isSouthOf(startTile)) match {
      case(true, false) => Some(South)
      case(false, true) => Some(North)
      case _ => None
    }

  override def nextPosition(direction: Direction): (Int, Int, Direction) =  direction match {
    case North => (xPos, yPos - 1, direction)
    case South => (xPos, yPos + 1, direction)
  }
}

case class SouthToEastPipe(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = 'L'

  override protected def startTileDirection(startTile: Tile): Option[Direction] =
    (isNorthOf(startTile), isWestOf(startTile)) match {
      case(true, false) => Some(South)
      case(false, true) => Some(East)
      case _ => None
    }

  override protected def nextPosition(direction: Direction) = direction match {
    case South => (xPos + 1, yPos, East)
    case West =>  (xPos, yPos -1, North)
  }
}

case class SouthToWestPipe(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = 'J'

  override protected def startTileDirection(startTile: Tile): Option[Direction] =
    (isNorthOf(startTile), isEastOf(startTile)) match {
      case(true, false) => Some(South)
      case(false, true) => Some(East)
      case _ => None
    }

  override protected def nextPosition(direction: Direction) = direction match {
    case South => (xPos - 1, yPos, West)
    case East =>  (xPos, yPos -1, North)
  }
}

case class NorthToEastPipe(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = 'F'

  override protected def nextPosition(direction: Direction) = direction match {
    case North => (xPos + 1, yPos, East)
    case West =>  (xPos, yPos + 1, South)
  }

  override protected def startTileDirection(startTile: Tile): Option[Direction] =
    (isWestOf(startTile), isSouthOf(startTile)) match {
      case (true, false) => Some(South)
      case (false, true) => Some(East)
      case _ => None
    }
}

case class NorthToWestPipe(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = '7'

  override protected def startTileDirection(startTile: Tile): Option[Direction] =
    (isEastOf(startTile), isNorthOf(startTile)) match {
      case(true, false) => Some(East)
      case(false, true) => Some(North)
      case _ => None
    }

  override protected def nextPosition(direction: Direction) = direction match {
    case North => (xPos - 1, yPos, West)
    case East =>  (xPos, yPos + 1, South)
  }
}

case class Ground(override val xPos: Int, override val yPos: Int) extends Tile {
  override val pipe: Char = '.'
}

def findStart(tiles: Array[Array[Tile]]) : Option[Tile] = {
0  def loopRows(row: Array[Array[Tile]] ): Option[Tile] = {
    Option(row.head).flatMap{ r => r.find(t => t.pipe == 'S') } match {
      case Some(start) => Some(start)
      case None => loopRows(row.tail)
    }
  }
  loopRows(tiles)
}

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day10.txt").getLines().toList
val rows = lines.length
val cols = lines.head.length
val tiles = Array.ofDim[Tile](rows, cols)

def findFurthestTile(startTiles: List[(Tile, Direction)]): Int = {
  def loopToTile(startOne: (Tile, Direction), startTwo: (Tile, Direction), seenOne: Int = 1, seenTwo: Int = 1): Int = {
    val (startTileOne, directionOne) = startOne
    val (startTileTwo, directionTwo) = startTwo
    println(s"seen $seenOne ($startTileOne $directionOne) ($startTileTwo $directionTwo)")
    if (startTileOne == startTileTwo) seenOne.max(seenTwo)
    else loopToTile(
      startTileOne.nextTile(tiles, directionOne),
      startTileTwo.nextTile(tiles, directionTwo),
      seenOne + 1,
      seenTwo + 1
    )
  }
  loopToTile(startTiles.head, startTiles.reverse.head)
}

lines.zipWithIndex.foreach {
  case (line, yPosition) =>
    line.zipWithIndex.foreach { case (pipe, xPosition) =>
      tiles(yPosition)(xPosition) = Tile(xPosition, yPosition, pipe)
    }
}

val x = for {
  start <- findStart(tiles)
  firstTiles = start.startTiles(tiles)
} yield findFurthestTile(firstTiles)
