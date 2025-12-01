package com.guardian.advent

import com.guardian.advent.twentyfour._

trait RawInputProvider[T] extends InputFileReader with SolutionHelpers {
  def rawInput: T
}

trait AdventOfCodePuzzle[T] {
  def rawSolution: List[T]
}

trait  December[A, S, T] extends RawInputProvider[S] with AdventOfCodePuzzle[T]  with Addables {

  def solver: Solver[T,A]
  def solve: A = {
    val t = rawSolution
    solver.solution(t)
  }

  protected def listSizeSolver: ListSizeSolver[T] = new ListSizeSolver[T] {}

  protected def listTotalSolver(seed: A, solverTest: Boolean = true)(implicit addableImp: Addable[A,A]): Solver[A, A] =
    new ListTotalSolution[A, A] {
      override implicit val addable: Addable[A, A] = addableImp
      override def foldSeed: A = seed
    }
}

trait DecemberTwentyFive[A, S, T] extends December[A,S,T] {
  override lazy val fileName: String = s"advent.2025/$resourceName"
}

trait PuzzleDebugger extends InputFileReader {
  def debugCase: Int

  override lazy val resourceName: String = s"debug/$day/day_${day}_debug_${debugCase}.txt"
}




