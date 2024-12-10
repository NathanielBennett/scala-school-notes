package com.guardian.advent.twentyfour

trait DecemberTwo extends AdventOfCode with App {

  override def day: Int = 2

  val reports = lineParser[List[Int]](true){
    case line =>
      val report = line.split(" ")
      .map(_.toInt).toList
      Some(report)
  }

  def recordToDiffs(record: List[Int]) : List[(Int, Int)] = {
      def diffs(ints: List[Int], prevHead: Int, acc: List[(Int, Int)] = Nil) : List[(Int, Int)]= {
        ints match {
          case Nil => acc
          case head :: tail =>
            diffs(tail, head, (head.compareTo(prevHead), Math.abs(head - prevHead)) :: acc)
        }
      }
    diffs(record.tail, record.head)
  }

  reports.foreach{
    x =>
      println(x)
      println(recordToDiffs(x))
  }
}

object DecTwoPartOne extends DecemberTwo