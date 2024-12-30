package com.guardian.advent.twentyfour


trait Solver[S, T] {

  def test: Boolean
  def solution(list: List[S]): T
}

trait ListSizeSolver[S] extends Solver[S,Int] {

  override def solution(list: List[S]): Int = list.length
}

trait ListTotalSolution[S, T] extends Solver[S,T] {

  def toFold(s: S, t: T): S
  def makeS: S
  def solution(list: List[T]): S = list.foldLeft(makeS) { case( s, t ) => toFold( s, t ) }
}

trait IntSolver extends ListTotalSolution[Int, Int] {

  override def makeS: Int = 0
  override def toFold(s: Int, t: Int): Int = s + t
}

trait PuzzleTest {
  def test = true
}

trait PuzzleSolution {
  def test: Boolean = false
}



