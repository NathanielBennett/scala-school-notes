package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.IntegerListParser
import com.guardian.advent.{AdventOfCodeParser, InputFileReader}

import scala.util.Try

trait DecemberTwoParser extends IntegerListParser  {

  override def lineParser(line: String): Option[List[Int]] = Try {
    line.split(" ").map(_.toInt).toList
  }.toOption

}

trait DecemberTwo extends DecemberTwoParser with InputFileReader {

  override def day: Int = 2

  val lines = getLines()
  val reports = parseLinesFromResource(lines)

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

trait DecemberTwoPartOne extends DecemberTwo {  
  override def solve(): Int =
    reports.filter { record => checkRecord(record) }.length
}

object DecemberTwoPartOneTest extends DecemberTwoPartOne {
  override def test: Boolean = true
}

object DecemberTwoPartOneSolution extends DecemberTwoPartOne {
  override def test: Boolean = false 
}

trait DecemberTwoPartTwo extends DecemberTwo {

  override def test: Boolean = true
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

  override def solve(): Int = reports.filter{
      report => checkRecord(report) || checkPemutations(report)
  }.size
}

object DecemberTwoPartTwoTest extends DecemberTwoPartTwo {
  override def test: Boolean = true
}

object DecemberTwoPartTwoSolution extends DecemberTwoPartTwo {
  override def test: Boolean = false
}
