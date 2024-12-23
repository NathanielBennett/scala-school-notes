package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCode

trait DecemberTwo extends AdventOfCode[Int] with App {

  override def day: Int = 2

  val reports = lineParser[List[Int]](){
    case line =>
      val report = line.split(" ")
        .map(_.toInt).toList
      Some(report)
  }

  def rowToDiffs(record: List[Int], lastHead: Int, acc: List[(Int, Int)] = Nil): List[(Int, Int)] = {
    record match {
      case Nil => acc
      case head :: tail => rowToDiffs(tail, head, (lastHead.compareTo(head), Math.abs(lastHead - head)) :: acc)
    }
  }

  protected def checkRecord(record: List[Int]): Boolean = {
    val diffs = rowToDiffs(record.tail, record.head)
    diffs.map(_._1).toSet.size == 1 && !diffs.exists{ d => d._2 > 3 }
  }
}
object DecTwoPartOneTest extends DecemberTwo {

  override def test: Boolean = true

  override def solve(): Int =
    reports.filter { record => checkRecord(record) }.length
}

object DecTwoPartTwoTest extends DecemberTwo {

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