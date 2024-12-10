package com.guardian.advent.twentyfour

trait DecemberTwo extends AdventOfCode with App {

  override def day: Int = 2

  val reports = lineParser[List[Int]](){
    case line =>
      val report = line.split(" ")
      .map(_.toInt).toList
      Some(report)
  }

  
  def checkReport(report: String): Boolean = {
    def check(ints: List[Int], last: Option[Int], ascending: Option[Int] ): Boolean =
      ints match {
        case Nil => true
        case head :: tail =>
          (ascending, last) match {
            case (None, None) => check(tail, Some(head), ascending)
            case (Some(l), None) =>
              if (Math.abs(head - l) > 3) false
              else check(tail, Some(head), Some(l.compareTo(head)))
            case(Some(l), Some(asc) ) =>

          }

      }
  }
}
