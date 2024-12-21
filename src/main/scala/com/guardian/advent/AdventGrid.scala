package com.guardian.advent

trait GridEntry[T] {
  def xPosition: Int
  def yPosition: Int
  def value: T
  def equalPosition(x: Int, y: Int): Boolean = x == xPosition && y == yPosition
}

trait Grid[T <: GridEntry[T]] {

  def defaultRange = (-1 to 1)
  def entries: Set[T]
  lazy val maxX = entries.maxBy{_.xPosition}.xPosition
  lazy val maxY = entries.maxBy(_.yPosition).yPosition

  private def neighbours(xRange: Range, yRange: Range): List[(Int, Int)] = {
    (for {
      y <- yRange
      x <- xRange
    } yield (x, y)).toList
  }

  private def getRow(row: Int) : List[T] = entries.filter{ t => t.yPosition == row}.toList.sortBy(_.xPosition)

  def neibouringEntries(t: T)(filter: (T, T) => Boolean = (_, _) => true): List[T] = {
      neighbours(defaultRange, defaultRange).flatMap{
        case (x, y) => entries.find( t => t.equalPosition(x, y) )
      }.filter{ entry => filter(t, entry) }
  }

  def printGrid(separator: Option[String] = None) : Unit = {
    (0 to maxY).toList.foreach{
      y =>
        val row = entries.filter(_.yPosition == y)
          .map{ t => t.value.toString }
          .mkString(separator.getOrElse( ""))
        println(row)
    }
  }
}
