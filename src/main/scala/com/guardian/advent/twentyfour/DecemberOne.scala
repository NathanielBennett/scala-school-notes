package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.IntegerTupleParser
import scala.util.Try

trait DecemberOneParser extends IntegerTupleParser {

  val figureMatcher = """^(\d+)\s+(\d+)$""".r

  override def lineParser(line: String): Option[(Int, Int)] = Try {
    val figureMatcher(left, right) = line
    (left.toInt, right.toInt)
  }.toOption
}

trait DecemberOne[L] extends December[Int, List[(Int, Int)], L] with DecemberOneParser {

  override val day = 1

  val (left, right) = rawInput
    .foldLeft((List[Int](), List[Int]())) {
      case ((leftList, rightList), (left, right)) => (left :: leftList, right :: rightList)
    }
}

trait DecemberOnePartOne extends DecemberOne[(Int, Int)]    {

  override def solver: Solver[(Int, Int), Int] = new ListTotalSolution[(Int, Int), Int] {
    override implicit val addable: Addable[(Int, Int), Int] = new Addable[(Int, Int), Int] {
      override def add(a: (Int, Int), b: Int): Int = {
        val (left, right) = a
        b + Math.abs(left - right)
      }
    }

    override def foldSeed: Int = 0
  }

  override def rawSolution: List[(Int, Int)] = left.sorted.zip(right.sorted)
}

trait DecemberOnePartTwo extends DecemberOne[Int] {

  override def solver: Solver[Int, Int] = listTotalSolver(0, test)

  override def rawSolution: List[Int] = {
    val totals = right.groupBy { k => k }.map { case (k, v) => (k, v.length) }
    left.flatMap { k =>totals.get(k).map { v => k * v} }
  }
}

object DecemberOnePartOneTest extends DecemberOnePartOne with PuzzleTest
object DecemberOnePartOneSolution extends DecemberOnePartOne with PuzzleSolution
object DecemberOnePartTwoTest extends DecemberOnePartTwo with PuzzleTest
object DecemberOnePartTwoSolution extends DecemberOnePartTwo with PuzzleSolution