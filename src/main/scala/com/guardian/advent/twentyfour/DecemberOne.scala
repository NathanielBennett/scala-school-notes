package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCode

import scala.util.Try

trait DecemberOne extends AdventOfCode with App {

  override val day = 1
  val figureMatcher = """^(\d+)\s+(\d+)$""".r

  val (left, right) = lineParser[(Int, Int)]() {
    line =>
      Try {
        val figureMatcher(left, right) = line
        (left.toInt, right.toInt)
      }.toOption
  }.foldLeft((List[Int](), List[Int]())) {
    case ((leftList, rightList), (left, right)) => (left :: leftList, right :: rightList)
  }
}

object PartOne extends DecemberOne {
  val total = left.sorted.zip(right.sorted)
    .foldLeft(0) {
      case (total, (left, right)) => total + Math.abs(left - right)
    }
  println(total)
}

object PartTwo extends DecemberOne {
  val totalsMap = right.groupBy{ k => k }
    .map{ case(k, v) => (k, v.length) }

  val total = left.flatMap{
      k => totalsMap.get(k).map{ v => k * v }
  }
    .foldLeft(0) {case(total, sum) => total + sum}

  println(total)
}



