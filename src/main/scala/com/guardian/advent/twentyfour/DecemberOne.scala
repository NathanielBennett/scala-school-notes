package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.IntegerTupleParser
import scala.util.Try

trait DecemberOneParser extends IntegerTupleParser {

  val figureMatcher = """^(\d+)\s+(\d+)$""".r

  override def lineParser(line: String): Option[(Int, Int)] =   Try {
    val figureMatcher(left, right) = line
    (left.toInt, right.toInt)
  }.toOption
}

trait DecemberOne extends December[(Int, Int), (Int, Int)] with DecemberOneParser with Solver[Int, (Int, Int)]{

  override val day = 1

  override def makeS  = 0

  override def toFold(total: Int, t: (Int, Int)): Int = {
    val (left, right) = t
    total + Math.abs(left - right)
  }

  val (left, right) = rawInput
    .foldLeft((List[Int](), List[Int]())) {
      case ((leftList, rightList), (left, right)) => (left :: leftList, right :: rightList)
    }
}

trait DecemberOnePartOne extends DecemberOne with Solver[Int, (Int, Int)]   {

  override def rawSolution: List[(Int, Int)] = left.sorted.zip(right.sorted)
}

trait DecemberOnePartTwo extends DecemberOne with Solver[Int, (Int,Int)] {

  override def rawSolution: List[(Int, Int)] = right.groupBy { k => k }.map { case (k, v) => (k, v.length) }.toList

}

object DecemberOnePartOneTest extends DecemberOnePartOne with PuzzleTest
object DecemberOnePartOneSolution extends DecemberOnePartOne with PuzzleSolution
object DecemberOnePartTwoTest extends DecemberOnePartTwo with PuzzleTest
object DecemberOnePartTwoSolution extends DecemberOnePartTwo with PuzzleSolution




