package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCode

trait DecemberTwo extends AdventOfCode with App {

  def getSafe: List[List[Int]]
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

  val safe = getSafe
  println(safe.size)
}
object DecTwoPartOne extends DecemberTwo {

  def getSafe: List[List[Int]] =
    reports.filter { record => checkRecord(record) }
}

object DecTwoPartTwo extends DecemberTwo {

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
  def getSafe: List[List[Int]] = reports.filter{
      report => checkRecord(report) || checkPemutations(report)
  }
}