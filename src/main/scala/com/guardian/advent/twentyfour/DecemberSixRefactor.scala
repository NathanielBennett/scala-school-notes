package com.guardian.advent.twentyfour

import com.guardian.advent.GridEntry

trait DecemberSixRefactor extends December[Int, CharGrid, GridEntry[Char]] with DecemberSixParser {

  override def day: Int = 6
  override def solver: Solver[GridEntry[Char], Int] = listSizeSolver

  val grid = rawInput
  val begin = grid.entries.collectFirst{
    case start: Start => start
  }

  def findPath(start: Start): List[GridEntry[Char]] = {

    val cardinal = start.cardinal


  }


}
