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
  def isRepitition(testId: String): Boolean

  override def rawSolution: List[Long] = {
    for {
      (start, end) <- rawInput
      testIdL <- (start to end).toList
      if isRepitition(testIdL.toString)
    } yield testIdL
  }
}

trait DecemberTwoPartOne extends DecemberTwo {

  override def isRepitition(testId: String): Boolean = {
    val length = testId.length
    if ( length % 2 == 1) false
    else {
      lazy val halfLen = length / 2
      val (left, right) = testId.splitAt(halfLen)
      left == right
    }
  }
}

object DecemberTwoPartOneTest extends DecemberTwoPartOne with PuzzleTest
object DecemberTwoPartOneSolution extends DecemberTwoPartOne with PuzzleSolution

trait DecemberTwoPartTwo extends DecemberTwo {

  override def isRepitition(testId: String): Boolean = {

     val halfLen = testId.length / 2

     def loop(groupSize: Int = 1): Boolean = {
       if (groupSize > halfLen) false
       else {
         val repeatSet = testId.grouped(groupSize).toSet
         repeatSet.size == 1 || loop(groupSize + 1)
       }
     }
     loop()
  }
}

object DecemberTwoPartTwoTest extends DecemberTwoPartTwo with PuzzleTest
object DecemberTwoPartTwoSolution extends DecemberTwoPartTwo with PuzzleSolution