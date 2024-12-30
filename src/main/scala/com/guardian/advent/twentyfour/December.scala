package com.guardian.advent.twentyfour

import com.guardian.advent.{InputFileReader, SolutionHelpers}

trait RawInputProvider[T] extends InputFileReader with SolutionHelpers {
  def rawInput: T
}

trait AdventOfCodePuzzle[T] {
  def rawSolution: List[T]
}

trait December[S, T] extends RawInputProvider[S] with AdventOfCodePuzzle[T]
