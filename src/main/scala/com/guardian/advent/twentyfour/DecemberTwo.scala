package com.guardian.advent.twentyfour

import com.guardian.advent.December
import com.guardian.advent.parsers.IntegerListParser

import scala.util.Try

trait DecemberTwoParser extends IntegerListParser  {

  override def separator: String = " "
}

trait DecemberTwo extends December[Int, List[List[Int]], List[Int]] with DecemberTwoParser {

  override def day: Int = 2

  override def solver: Solver[List[Int], Int] = listSizeSolver

  val reports = rawInput

  def rowToDiffs(record: List[Int], lastHead: Int, acc: List[(Int, Int)] = Nil): List[(Int, Int)] = {
    record match {
      case Nil => acc
      case head :: tail => rowToDiffs(tail, head, (lastHead.compareTo(head), Math.abs(lastHead - head)) :: acc)
    }
  }

  protected def checkRecord(record: List[Int]): Boolean = {
    val diffs = rowToDiffs(record.tail, record.head)
    diffs.map(_._1).toSet.size == 1 && !diffs.exists { d => d._2 > 3 }
  }
}

trait DecemberTwoPartOne extends DecemberTwo  {
  override def rawSolution: List[List[Int]] = reports.filter { record => checkRecord(record) }
}

trait DecemberTwoPartTwo extends DecemberTwo {

  def permute(ints: List[Int], indexToFilter: Int = 0) : List[Int] =
    ints.zipWithIndex.flatMap {
      case (level, index) =>
        if (index == indexToFilter) None
        else Some(level)
  }

  def checkPemutations(record: List[Int], currentToCheck: Int = 0): Boolean = {

     if (currentToCheck == record.size + 1) false
     else {
       val pernutation = permute(record, currentToCheck)
       checkRecord(pernutation) || checkPemutations(record, currentToCheck + 1)
     }
  }

  override def rawSolution: List[List[Int]] = reports.filter { record => checkRecord(record) || checkPemutations(record) }
}

object DecemberTwoPartOneTest extends DecemberTwoPartOne with PuzzleTest
object DecemberTwoPartOneSolution extends DecemberTwoPartOne with PuzzleSolution
object DecemberTwoPartTwoTest extends DecemberTwoPartTwo with PuzzleTest
object DecemberTwoPartTwoSolution extends DecemberTwoPartTwo with PuzzleSolution