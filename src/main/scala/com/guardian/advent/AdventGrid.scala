package com.guardian.advent

trait GridEntry[T] {
  def xPosition: Int
  def yPosition: Int
  def value: T
  def equalPosition(x: Int, y: Int): Boolean = x == xPosition && y == yPosition
}

trait Grid[T] {

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

  def neibouringEntries(gridEntry: GridEntry[T])(filter: (GridEntry[T], GridEntry[T]) => Boolean = (_, _) => true): List[GridEntry[T] = {
      neighbours(defaultRange, defaultRange).flatMap{
        case (x, y) => entries.find( t => t.equalPosition(x, y) )
      }.filter{ entry => filter(  gridEntry, entry )  }
  }

  def printGrid(separator: Option[String] = None) : Unit = {
    (0 to maxY).toList.foreach{
      y =>
        val row = entries.filter(_.yPosition == y)
          .map{ t => t.value.toString }
          .mkString(separator.getOrElse(""))
        println(row)
    }
  }
}
