package com.guardian.advent.twentyfive

import com.guardian.advent.DecemberTwentyFive
import com.guardian.advent.parsers.GenericSingleLineTupleParser
import com.guardian.advent.twentyfour.{PuzzleSolution, PuzzleTest, Solver}

trait DecemberTwpParser extends GenericSingleLineTupleParser[Long, Long] {

  private def toLong(s: String): Long = s.toLong

  override def stringToA(s: String): Long = toLong(s)
  override def stringToB(s: String): Long = toLong(s)
  override def separate(string: String): List[String] = string.split(",").toList

  override def lineParser(line: String): Option[(Long, Long)] = {
     val list = line.split("-").toList
     listToTuple(list)
  }
}

trait DecemberTwo extends DecemberTwentyFive[Long, List[(Long, Long)], Long] with DecemberTwpParser {

  override def day: Int = 2

  override def solver: Solver[Long, Long] = listTotalSolver(0L)

  override def rawSolution: List[Long] = {
    for {
      (start, end) <- rawInput
      testIdL <- (start to end).toList
      if isRepitition(testIdL.toString)
    } yield testIdL
  }

  protected def isRepitition(testId: String): Boolean = {
    val length = testId.length
    if ( length % 2 == 1) false
    else {
      lazy val halfLen = length / 2
      val (left, right) = testId.splitAt(halfLen)
      left == right
    }
  }
}

trait DecemberTwoPartOne extends DecemberTwo

object DecemberTwoPartOneTest extends DecemberTwoPartOne with PuzzleTest
object DecemberTwoPartOneSolution extends DecemberTwoPartOne with PuzzleSolution