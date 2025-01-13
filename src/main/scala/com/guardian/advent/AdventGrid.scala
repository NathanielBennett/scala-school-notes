package com.guardian.advent

import io.opencensus.metrics.LabelKey

sealed trait Direction {
  def nextGridCoords(x: Int, y: Int): (Int, Int)
}

trait Cardinal extends Direction {
  def nextCardinal: Cardinal
}
trait SemiCardinal extends Direction

trait Directions {
  def allDirections = List(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
  def cardinalDirects: List[Direction] = allDirections.collect {
    case cardinal: Cardinal => cardinal
  }
  def nonCardinals: List[Direction] = allDirections.collect{
    case semiCardinal: SemiCardinal => semiCardinal
  }
  def oneDiagonal: Set[Direction] = Set(NorthWest, SouthEast)
}

case object North extends Cardinal {
  override def nextCardinal: Cardinal = East
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x, y - 1)
}

case object NorthEast extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y - 1)
}

case object East extends Cardinal {
  override def nextCardinal: Cardinal = South
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y)
}

case object SouthEast extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y + 1)
}

case object South extends Cardinal {
  override def nextCardinal: Cardinal = West
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 0, y + 1)
}

case object SouthWest extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y + 1)
}

case object West extends Cardinal {
  override def nextCardinal: Cardinal = North
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y)
}
case object NorthWest extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y - 1)
}

trait GridEntry[T] {
  def xPosition: Int
  def yPosition: Int
  def value: T
  def nextCoords(direction: Direction): (Int,Int) = direction.nextGridCoords(xPosition, yPosition)
  def equalPosition(x: Int, y: Int): Boolean = x == xPosition && y == yPosition
}

trait Grid[T] extends Directions {

  def defaultRange = (-1 to 1)
  def entries: Set[GridEntry[T]]
  lazy val maxX = entries.maxBy{_.xPosition}.xPosition
  lazy val maxY = entries.maxBy(_.yPosition).yPosition
  private def neighbours(xRange: Range, yRange: Range): List[(Int, Int)] = {
    (for {
      y <- yRange
      x <- xRange
    } yield (x, y)).toList
  }

  private def getRow(row: Int) : List[GridEntry[T]] = entries.filter{ t => t.yPosition == row}.toList.sortBy(_.xPosition)

  def vertice(start: GridEntry[T], direction: Direction)( isLast: List[GridEntry[T]] => Boolean ): List[GridEntry[T]] = {
     def nextEntry(entry: GridEntry[T], acc: List[GridEntry[T]]): List[GridEntry[T]] = {
       val tail = (entry :: acc)
       if (isLast(tail)) tail.reverse
       else {
         val (nextX, nextY) = direction.nextGridCoords(entry.xPosition, entry.yPosition)
         entries.find(ge => ge.xPosition == nextX && ge.yPosition == nextY).map {
           next => nextEntry(next, tail)
         }.getOrElse { (entry :: acc).reverse }
       }
     }
     nextEntry(start, Nil)
  }

  def filterEntries(filter: GridEntry[T] => Boolean): List[GridEntry[T]] = entries.filter(filter).toList

  def neibouringEntries(gridEntry: GridEntry[T])(filter: (GridEntry[T], GridEntry[T]) => Boolean = (_, _) => true): List[GridEntry[T]] = {
      neighbours(defaultRange, defaultRange).flatMap{
        case (x, y) => entries.find( t => t.equalPosition(x, y) )
      }.filter{ entry => filter(  gridEntry, entry )  }
  }

  def getNeighbours(gridEntry: GridEntry[T], directions: List[Direction]): List[GridEntry[T]] =
    getNeigboursAndDirections(gridEntry, directions).map {case (entry, _) => entry}

  def getNeigboursAndDirections(gridEntry: GridEntry[T], directions: List[Direction]): List[(GridEntry[T], Direction)] =
    directions.flatMap {
      direction =>
        val (xPosition, yPosition) = gridEntry.nextCoords(direction)
        entries.find { ge => ge.xPosition == xPosition && ge.yPosition == yPosition }.map { entry =>
          (entry, direction)
        }
    }

  def printGrid(separator: Option[String] = None) : Unit = {
    (0 to maxY).toList.foreach{
      y =>
        val rowEntries = entries.filter(_.yPosition == y).toList.sortBy(_.xPosition)
        val row = rowEntries.map{ t => t.value.toString }
          .mkString(separator.getOrElse(""))
        println(row)
    }
  }
}

