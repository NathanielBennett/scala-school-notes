package com.guardian.advent.grid

import com.guardian.advent.SolutionHelpers

sealed trait Direction {
  def nextGridCoords(x: Int, y: Int): (Int, Int)
  def prevGridCoords(x: Int, y: Int): (Int, Int)
}

trait Cardinal extends Direction {
  def nextCardinal: Cardinal
  def previousCardinal: Cardinal
  def counterCardinal: Cardinal
  def isPolar: Boolean
}

object Cardinal {
  def apply(s: String): Option[Cardinal] = Option(s).collect {
    case "North" => North
    case "East" => East
    case "South"  => South
    case "West" => West
  }

  def apply(ch: Char): Option[Cardinal] = Option(ch).collect {
    case '^' => North
    case '>' => East
    case '<' => West
    case 'v' => South
  }
}

trait SemiCardinal extends Direction

trait Directions {
  def allDirections: List[Direction] = List(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
  def cardinals: List[Cardinal] = List(North, South, East, West)
  def nonCardinals: List[Direction] = allDirections.collect{
    case semiCardinal: SemiCardinal => semiCardinal
  }
  def oneDiagonal: Set[Direction] = Set(NorthWest, SouthEast)
}

case object North extends Cardinal {
  override def nextCardinal: Cardinal = East
  override def previousCardinal: Cardinal = West
  override def counterCardinal: Cardinal = South
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x, y - 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x, y + 1)
  override def isPolar: Boolean = true
}

case object NorthEast extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y - 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y + 1)
}

case object East extends Cardinal {
  override def nextCardinal: Cardinal = South
  override def previousCardinal: Cardinal = North
  override def counterCardinal: Cardinal = West
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y)
  override def isPolar: Boolean = false
}

case object SouthEast extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y + 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y - 1)
}

case object South extends Cardinal {
  override def nextCardinal: Cardinal = West
  override def previousCardinal: Cardinal = East
  override def counterCardinal: Cardinal = North
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x + 0, y + 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x + 0, y - 1)
  override def isPolar: Boolean = true
}

case object SouthWest extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y + 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y - 1)
}

case object West extends Cardinal {
  override def nextCardinal: Cardinal = North
  override def previousCardinal: Cardinal = South
  override def counterCardinal: Cardinal = East
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y)
  override def isPolar: Boolean = false
}
case object NorthWest extends SemiCardinal {
  override def nextGridCoords(x: Int, y: Int): (Int, Int) = (x - 1, y - 1)
  override def prevGridCoords(x: Int, y: Int): (Int, Int) = (x + 1, y + 1)
}

trait GridEntry[T] {
  def xPosition: Int
  def yPosition: Int
  def value: T
  def nextCoords(direction: Direction): (Int,Int) = direction.nextGridCoords(xPosition, yPosition)
  def previousCoords(direction: Direction): (Int, Int) = direction.prevGridCoords(xPosition, yPosition)
  def equalPosition(x: Int, y: Int): Boolean = x == xPosition && y == yPosition
  def equalPosition(gridEntry: GridEntry[T]): Boolean = gridEntry.xPosition == xPosition && gridEntry.yPosition == yPosition
  def point: (Int, Int) = (xPosition, yPosition)
  def abs(gridEntry: GridEntry[T]): (Int, Int) = (Math.abs(xPosition - gridEntry.xPosition), Math.abs(yPosition - gridEntry.yPosition))
  def plus(gridEntry: GridEntry[T]): (Int, Int) = plus(gridEntry.xPosition, gridEntry.yPosition)
  def plus(pos: (Int, Int)): (Int, Int) = {
    val (x, y) = pos
    (xPosition + x, yPosition + y)
  }

  def minus(gridEntry: GridEntry[T]): (Int, Int) = minus(gridEntry.xPosition, gridEntry.yPosition)
  def minus(pos: (Int, Int)): (Int, Int) = {
    val (x, y) = pos
    (xPosition - x, yPosition - y)
  }

  def equalX(gridEntry: GridEntry[T]): Boolean = gridEntry.xPosition == this.xPosition
  def equalY(gridEntry: GridEntry[T]): Boolean = gridEntry.yPosition == this.yPosition

  def westPont: (Int, Int) = (-xPosition, yPosition)
//  def is[A <: GridEntry[T]]: Boolean = Option(this).collectFirst{ a: A => a }.isDefined

}

trait Grid[T] extends Directions with SolutionHelpers {

  def defaultRange = (-1 to 1)
  def entries: Set[GridEntry[T]]

  lazy val maxX = entries.maxBy{_.xPosition}.xPosition
  lazy val maxY = entries.maxBy(_.yPosition).yPosition
  lazy val size = entries.size

  lazy private val orderedRows: List[(Int, Set[GridEntry[T]])] =
    entries.groupBy{ entry => entry.yPosition }
      .toList
      .sortBy{ case (row, _) => row}


  private def neighbours(xRange: Range, yRange: Range): List[(Int, Int)] = {
    (for {
      y <- yRange
      x <- xRange
    } yield (x, y)).toList
  }

  def getRow(row: Int) : List[GridEntry[T]] = entries.filter{ t => t.yPosition == row}.toList.sortBy(_.xPosition)

  private def edgeX(xPosition: Int): Boolean = xPosition == 0 || xPosition == maxX
  private def edgeY(yPosition: Int): Boolean = yPosition == 0 || yPosition == maxY

  def isEdge(gridEntry: GridEntry[T]): Boolean = edgeX(gridEntry.xPosition) || edgeY(gridEntry.yPosition)
  def endsEdge(vertice: List[GridEntry[T]]): Boolean = vertice.headOption match {
    case Some(gridEntry) => isEdge(gridEntry)
    case None => false
  }

  def sortedEntries: List[GridEntry[T]] = entries.toList.sortBy( gridEntry => (gridEntry.yPosition, gridEntry.xPosition) )

  def vertice(start: GridEntry[T], direction: Direction)( isLast: (GridEntry[T], List[GridEntry[T]]) => Boolean ): List[GridEntry[T]] = {
     def nextEntry(entry: GridEntry[T], acc: List[GridEntry[T]]): List[GridEntry[T]] = {
       val tail = (entry :: acc)
       if (isLast(entry, acc)) acc
       else {
         val (nextX, nextY) = direction.nextGridCoords(entry.xPosition, entry.yPosition)
         entries.find(ge => ge.xPosition == nextX && ge.yPosition == nextY).map {
           next => nextEntry(next, tail)
         }.getOrElse { (entry :: acc ) }
       }
     }
     nextEntry(start, Nil)
  }

  def nextMatchingEntry(start: GridEntry[T], direction: Direction, matchesLast: GridEntry[T] => Boolean): Option[GridEntry[T]] = {
      nextEntryByDirection(start, direction) match {
        case Some(entry) =>
          if (matchesLast(entry)) Some(entry)
          else nextMatchingEntry(entry, direction, matchesLast)
        case _ => None
      }
  }

  def nextEntryByDirection(gridEntry: GridEntry[T], direction: Direction): Option[GridEntry[T]] = {
      val (nextX, nextY) = gridEntry.nextCoords(direction)
      entries.find{ e => e.xPosition == nextX && e.yPosition == nextY}
  }

  def previousEntryByDirection(gridEntry: GridEntry[T], direction: Direction): Option[GridEntry[T]] = {
    val (nextX, nextY) = gridEntry.previousCoords(direction)
    entries.find{ e => e.xPosition == nextX && e.yPosition == nextY}
  }

  def filterEntries(filter: GridEntry[T] => Boolean): List[GridEntry[T]] = entries.filter(filter).toList

  def neibouringEntries(gridEntry: GridEntry[T])(filter: (GridEntry[T], GridEntry[T]) => Boolean = (_, _) => true): List[GridEntry[T]] = {
      neighbours(defaultRange, defaultRange).flatMap{
        case (x, y) => entries.find( t => t.equalPosition(x, y) )
      }.filter{ entry => filter(  gridEntry, entry )  }
  }

  def getNeighbours(gridEntry: GridEntry[T], directions: List[Direction]): List[GridEntry[T]] =
    getNeigboursAndDirections(gridEntry, directions).map {case (entry, _) => entry}

  def getFilteredNeighbours(gridEntry: GridEntry[T], directions: List[Direction])(filterEntries: GridEntry[T] => Boolean) : List[GridEntry[T]] =
    getNeighbours(gridEntry, directions).filter(filterEntries)

  def getNeigboursAndDirections(gridEntry: GridEntry[T], directions: List[Direction] = allDirections): List[(GridEntry[T], Direction)] =
    directions.flatMap {
      direction =>
        val (xPosition, yPosition) = gridEntry.nextCoords(direction)
        entries.find { ge => ge.xPosition == xPosition && ge.yPosition == yPosition }.map { entry =>
          (entry, direction)
        }
    }

  def findEntry(xPosition: Int, yPosition: Int): Option[GridEntry[T]] = entries.find { entry => entry.xPosition == xPosition && entry.yPosition == yPosition }
  def findEntry(entryValue: T): Option[GridEntry[T]] = entries.find{ entry => entry.value == entryValue}


  def matchingEntries(filter: Set[GridEntry[T]] => Set[GridEntry[T]]): Set[GridEntry[T]] = filter(entries)

  def printGrid(separator: Option[String] = None) : Unit = {
    (0 to maxY).toList.foreach{
      y =>
        val rowEntries = entries.filter(_.yPosition == y).toList.sortBy(_.xPosition)
        val row = rowEntries.map{ t => t.value.toString }
          .mkString(separator.getOrElse(""))
        println(s"$row")
    }
    println()

  }

  def hasRowMatching(testEntries: Set[GridEntry[T]] = this.entries)(testList: List[GridEntry[T]] => Boolean): Boolean = {
    def loop(toTest: List[List[GridEntry[T]]]): Boolean = {
      toTest match {
        case Nil => false
        case head :: tail =>
          val rowMatches = testList(head)
          if (rowMatches) true
          else loop(tail)
      }
    }
    val rows = orderedRows.map{ case(_, row) => row.toList }
    loop(rows)
  }

  def printGridDebug(gridEntries: List[GridEntry[T]], concat: String = " " ): Unit = {
   // println(gridEntries)
    println()
    (0 to maxY).toList.foreach{
      y =>
        val rowEntries = entries.filter(_.yPosition == y).toList.sortBy(_.xPosition)
        val row = rowEntries.map{ t =>
           if ( gridEntries.contains(t)) "0"
           else t.value.toString
        }.mkString(concat)
        println(row)
    }
    println()
  }

  def printGridPathDebug(start: GridEntry[T], entriesAndCardninals: List[(GridEntry[T], Cardinal)]) = {
      (0 to maxY).toList.foreach{
        y =>
          val rowEntries = entries.filter(_.yPosition == y).toList.sortBy(_.xPosition)
          val row = rowEntries.foldLeft(new StringBuilder()) { case (sb, t) =>
            val s = entriesAndCardninals.find { case (entry, _) => entry == t }.map {
                case (_, cardinal) => cardinal match {
                  case North => "^"
                  case East => ">"
                  case South => "v"
                  case West => "<"
                }
              }.getOrElse(t.value.toString)
              if(t == start) sb.append(s"0") else sb.append(s"$s")
            }
          println(row)
      }
    }
}

