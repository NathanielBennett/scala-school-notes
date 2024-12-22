package com.guardian.advent

sealed trait Direction {
  def isCardinal: Boolean
  def nextGridCoords(x: Int, y: Int): (Int, Int)
}

trait Directions {
  def allDirections = List(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
  def cardinalDirections = allDirections.filter{ d => d.isCardinal }
}

case object North extends Direction {
  override def isCardinal: Boolean = true
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x, y - 1)
}

case object NorthEast extends Direction {
  override def isCardinal: Boolean = false
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y - 1)
}

case object East extends Direction {
  override def isCardinal: Boolean = true
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y)
}

case object SouthEast extends Direction {
  override def isCardinal: Boolean = false
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y + 1)
}

case object South extends Direction {
  override def isCardinal: Boolean = true
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 0, y + 1)
}

case object SouthWest extends Direction {
  override def isCardinal: Boolean = false
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y + 1)
}

case object West extends Direction {
  override def isCardinal: Boolean = true
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y)
}
case object NorthWest extends Direction {
  override def isCardinal: Boolean = false
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y - 1)
}

trait GridEntry[T] {
  def xPosition: Int
  def yPosition: Int
  def value: T
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

  def vertice(start: GridEntry[T], direction: Direction,  isLast: List[GridEntry[T]] => Boolean ): List[GridEntry[T]] = {
     def nextEntry(entry: GridEntry[T], acc: List[GridEntry[T]]): List[GridEntry[T]] = {
       val tail = entry :: acc
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

