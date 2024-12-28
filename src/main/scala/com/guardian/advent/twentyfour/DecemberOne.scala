package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.IntegerTupleParser
import com.guardian.advent.{AdventOfCodeParser, AdventOfCodePuzzle, InputFileReader}

import scala.::
import scala.collection.AbstractSeq
import scala.util.Try



trait DecemberOneParser extends IntegerTupleParser {

  val figureMatcher = """^(\d+)\s+(\d+)$""".r

  override def lineParser(line: String): Option[(Int, Int)] =   Try {
    val figureMatcher(left, right) = line
    (left.toInt, right.toInt)
  }.toOption
}

trait DecemberOne extends December[(Int, Int)] with DecemberOneParser {

   override val day = 1

   val (left, right) = rawInput()
    .foldLeft((List[Int](), List[Int]())) {
      case ((leftList, rightList), (left, right)) => (left :: leftList, right :: rightList)
    }
}

trait DecemberOnePartOne extends DecemberOne with Solver[Int, (Int, Int)]   {

  override def rawSolution: List[(Int, Int)]  = left.sorted.zip(right.sorted)

  override def makeS  = 0





      .foldLeft(0) { case (total, (left, right)) => total + Math.abs(left - right) }
  }
}

object DecemberOnePartOneTest extends DecemberOnePartOne with IntSolver with TestSolver[Int]

object DecemberOnePartOneSolution extends DecemberOnePartOne {
  override def test = false
}

trait DecemberOnePartTwo extends DecemberOne {

  def solve(): Int = {
    val totalsMap = right.groupBy { k => k }
      .map { case (k, v) => (k, v.length) }

    left.flatMap {
      k => totalsMap.get(k).map { v => k * v }
    }.foldLeft(0) { case (total, sum) => total + sum }
  }
}

object DecemberOnePartTwoTest extends DecemberOnePartTwo {
  override def test = true
}

object DecemberOnePartTwoSolution extends DecemberOnePartTwo {
  override def test = false
}




