package com.guardian.advent.grid

case class CharEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char) extends GridEntry[Char]
case class CharGrid(override val entries: Set[GridEntry[Char]]) extends Grid[Char] {
  def update( updateEntries: Set[GridEntry[Char]] ): CharGrid = CharGrid( updateEntries )
}
