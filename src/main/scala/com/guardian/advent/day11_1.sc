import scala.io.Source
import scala.util.Try

sealed trait Space {
  def xPos: Int
  def yPos: Int
}

object Space {
  def apply(c: Char, xPos: Int, yPos: Int): Space = c match {
    case '#' => Galaxy(xPos, yPos)
    case _ => Empty(xPos, yPos)
  }
}

//case class Pos(row: Int, col: Int)
case class Empty(override val xPos: Int, override val yPos: Int) extends Space
case class Galaxy(override val xPos: Int, override val yPos: Int) extends Space

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day11.txt").getLines().toList
val galaxies = lines.zipWithIndex.flatMap{
  case(line, yPos) => line.zipWithIndex.map{ case(c, xPos) => Space(c, yPos, xPos)}.toList
}
val maxX = galaxies.maxBy( g => g.xPos).xPos
val maxY = galaxies.maxBy( g => g.yPos).yPos

def minLine(galaxyA: Galaxy, galaxyB: Galaxy, emptyRows: Set[Int], emptyCols: Set[Int], multiplier: Int ): Long = {
  val xLen: Long = Math.abs(galaxyA.xPos - galaxyB.xPos) + (multiplier * emptyRows.count { r => (r > galaxyA.xPos && r < galaxyB.xPos)  || ( r < galaxyB.xPos && r > galaxyA.xPos )})
  val yLen: Long = Math.abs(galaxyA.yPos - galaxyB.yPos) + (multiplier * emptyCols.count { c => (c > galaxyA.yPos && c < galaxyB.yPos) || ( c > galaxyB.yPos && c < galaxyA.yPos )})
  xLen + yLen
}

val emptyRows = (0 to maxY).flatMap { row =>
   galaxies.collectFirst { case galaxy: Galaxy if (galaxy.xPos == row) => galaxy }
     .map{_ => None}.getOrElse(Some(row))
}.toSet

val emptyColumns = (0 to maxY).flatMap{ col =>
  galaxies.collectFirst{ case galaxy: Galaxy if (galaxy.yPos == col) => galaxy}
    .map{_ => None}.getOrElse(Some(col))
}.toSet

val unsortedPairs = galaxies.collect{ case galaxy: Galaxy => galaxy }
 // .map{ case galaxy: Galaxy => (galaxy.xPos, galaxy.yPos) }
  .combinations(2).toSet.toList
  .flatMap{l: List[Galaxy] =>  Try {l match {
        case List(a, b) => (a, b)
      }
    }.toOption
  }
  .map { case(galaxyA, galaxyB) => minLine(galaxyA, galaxyB, emptyRows, emptyColumns, 999999)}
  .sum