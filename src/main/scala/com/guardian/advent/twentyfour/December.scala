package com.guardian.advent.twentyfour

import com.guardian.advent.{InputFileReader, SolutionHelpers}

trait RawInputProvider[T] extends InputFileReader with SolutionHelpers {
  def rawInput: T
}

trait AdventOfCodePuzzle[T] {
  def rawSolution: List[T]
}

trait December[A, S, T] extends RawInputProvider[S] with AdventOfCodePuzzle[T]  with Addables {

  def solver: Solver[T,A]
  def solve: A = {
    val t = rawSolution
    solver.solution(t)
  }

  protected def listSizeSolver = new ListSizeSolver[T] {}

  protected def listTotalSolver(seed: A, solverTest: Boolean)(implicit addableImp: Addable[A,A]): Solver[A, A] =
    new ListTotalSolution[A, A] {
      override implicit val addable: Addable[A, A] = addableImp
      override def foldSeed: A = seed
    }
}

trait PuzzleDebugger extends InputFileReader {
  def debugCase: Int

  override lazy val resourceName: String = s"debug/$day/day_${day}_debug_${debugCase}.txt"
}




