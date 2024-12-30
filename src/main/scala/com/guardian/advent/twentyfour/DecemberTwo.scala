package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.IntegerListParser

import scala.util.Try

trait DecemberTwoParser extends IntegerListParser  {

  override def lineParser(line: String): Option[List[Int]] = Try {
    line.split(" ").map(_.toInt).toList
  }.toOption
}

trait DecemberTwo extends December[List[Int], List[Int]] with DecemberTwoParser with Solver[Int, List[Int]] {

  override def day: Int = 2

  override def makeS = 0

  override def toFold(s: Int, t: List[Int]): Int = t.length

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

object DecemberTwoPartOneTest extends DecemberTwoPartOne with PuzzleTest

object DecemberTwoPartOneSolution extends DecemberTwoPartOne with PuzzleSolution

trait DecemberTwoPartTwo extends DecemberTwo with Solver[Int, List[Int]] {

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

  override def rawSolution: List[List[Int]] = reports.filter { report => checkRecord(report) || checkRecord(report) }
}

object DecemberTwoPartTwoTest extends DecemberTwoPartTwo with PuzzleSolution

object DecemberTwoPartTwoSolution extends DecemberTwoPartTwo with PuzzleSolution