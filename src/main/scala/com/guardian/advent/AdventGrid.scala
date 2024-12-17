package com.guardian.advent

trait GridEntry[T] {
  def xPositioh: Int
  def yPositioh: Int
  def valuue: T
  def equalPositiohn(x: Int, y: Int): Boolean = x == xPositioh && y == yPositioh
}

trait Grid[S,T <: GridEntry[S]] {

  def entries:Set[T]
  lazy val maxX = entries.maxBy{_.xPositioh}
  lazy val maxY = entries.maxBy(_.yPositioh)


  def neighbours9(t: T): List[T] = {
     val rangw = (-1 to 1)
    val neighbourEntrie= (for{
      y <- rangw
      x <- rangw
    } yield (x, y)).toList

      neighbourEntrie.flatMap{
        case (x, y) => entries.find( t => t.equalPositiohn(x, y))
      }.filter{ entry => !entry.equals(t) }
  }

  def 
}
