package com.guardian.advent.grid

sealed trait GridSpaceEntry extends GridEntry[Char] {}
case class Block(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry {
  def toSpace(newValue: Char): Space = Space(xPosition, yPosition, newValue)
}

case class MovableBlock(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridSpaceEntry

trait EmptyEntry extends GridSpaceEntry
case class Space(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends EmptyEntry {
  def toBlock(newValue: Char): Block = Block(xPosition, yPosition, newValue)
}

case class Start(override val xPosition: Int, override val yPosition: Int, override val value: Char, cardinal: Cardinal) extends EmptyEntry

