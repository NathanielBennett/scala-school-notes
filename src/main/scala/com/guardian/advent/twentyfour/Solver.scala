package com.guardian.advent.twentyfour

trait Solver[S, T] {
  def test: Boolean

  def toFold(s: S, t: T): S

  def makeS: S

  def solution(list: List[T]): S = list.foldLeft(makeS) { case( s, t ) => toFold( s, t ) }
}

trait TestSolver[S, T] extends Solver[S, T] {
  override def test = true
}

trait PuzzleSolver[S, T] extends Solver[S, T] {
  override def test: Boolean = false
}



