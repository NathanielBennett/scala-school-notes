package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeGridParser, Cardinal, East, GridEntry, North, South, West}

sealed trait GridSpaceEntry extends GridEntry[Char]
case class Block(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry
case class Space(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry {
  def direction: Option[Cardinal] = value match {
    case '^' => Some(North)
    case '>' => Some(East)
    case 'v' => Some(South)
    case '<' => Some(West)
    case _ => None
  }
}

object GridSpaceEntry {
   def apply(xPosition: Int, yPosition: Int, char: Char): Option[GridSpaceEntry] =
     char match {
       case '.' => Some(Space(xPosition, yPosition, char))
       case '>' => Some(Space(xPosition, yPosition, char))
       case '<' => Some(Space(xPosition, yPosition, char))
       case 'v' => Some(Space(xPosition, yPosition, char))
       case '^' => Some(Space(xPosition, yPosition, char))
       case '#' => Some(Block(xPosition, yPosition, char))
       case _ => None
     }
}

tr ait DecemberSixParser extends AdventOfCodeGridParser[Char, CharGrid] {
  override def entryParser(x: Int, y: Int, value: Char): Option[GridEntry[Char]] = GridSpaceEntry(x, y, value)
  override def gridMaker(entries: Set[GridEntry[Char]]): CharGrid = CharGrid(entries)
}

trait DecemberSix extends December[Int, CharGrid, Int] {

  override def day: Int = 6
  override def solver: Solver[Int, Int] = listSizeSolver

  val grid = rawInput
  val begin = grid.entries
    .collectFirst {
      case space: Space => space.direction.map { case direction => (space, direction)}
    }.flatten



}
