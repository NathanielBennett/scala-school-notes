package com.guardian.advent.twentyfour

trait Addable[A, B] {
  def  add(a: A, b: B): B
}

trait Addables {

  implicit object AddLong extends Addable[Long, Long] {
    override def add(a: Long, b: Long): Long = a + b
  }

  implicit object AddInt extends Addable[Int, Int] {
    override def add(a: Int, b: Int): Int = a + b
  }
}

trait Solver[S, T] extends Addables {
  def solution(list: List[S]): T
}

trait ListSizeSolver[S] extends Solver[S,Int] {
  override def solution(list: List[S]): Int = list.length
}

trait ListTotalSolution[S, T] extends Solver[S,T] {

  implicit val addable: Addable[S, T]

  def foldSeed: T
  override def solution(list: List[S]) : T = makeSolution(list)

  private def makeSolution(list: List[S])(implicit addable : Addable[S, T]): T = list.foldLeft(foldSeed) {
    case( t, s ) => addable.add(s, t)
  }
}

trait IntSolver extends ListTotalSolution[Int, Int] {
  override def foldSeed: Int = 0
}

trait PuzzleTest {
  def test: Boolean = true
}

trait PuzzleSolution {
  def test: Boolean = false
}



