package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeParser, InputFileReader}

import scala.::
import scala.collection.AbstractSeq
import scala.util.Try

trait AdventDat

trait DecemberOneParser extends AdventOfCodeParser[(Int, Int), List[(Int, Int)]] {

  val figureMatcher = """^(\d+)\s+(\d+)$""".r

  override def lineParser(line: String): Option[(Int, Int)] =   Try {
    val figureMatcher(left, right) = line
    (left.toInt, right.toInt)
  }.toOption

  override def sequenceToCollection(seq: Seq[(Int, Int)]): List[(Int, Int)] = seq.toList

  override def toSeq(list: List[String]): AbstractSeq[String] = list
}

trait DecemberOne extends DecemberOneParser with InputFileReader {

  override val day = 1

   val lines = getLines()
   val (left, right) = parseLinesFromResource(lines)
    .foldLeft((List[Int](), List[Int]())) {
      case ((leftList, rightList), (left, right)) => (left :: leftList, right :: rightList)
    }
}

trait DecemberOnePartOne extends DecemberOne {
  override def solve() : Int = {
    left.sorted.zip(right.sorted)
      .foldLeft(0) { case (total, (left, right)) => total + Math.abs(left - right) }
  }
}

object DecemberOnePartOneTest extends DecemberOnePartOne {
  override def test = true
}

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




