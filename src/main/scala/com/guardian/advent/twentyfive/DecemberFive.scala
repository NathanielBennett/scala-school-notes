package com.guardian.advent.twentyfive

import com.guardian.advent.{AdventOfCodeInstructionsParser, AdventOfCodeParser, DecemberTwentyFive}
import com.guardian.advent.parsers.{GenericTupleParser, SingleValueParser}
import com.guardian.advent.twentyfour.{PuzzleSolution, PuzzleTest, Solver}

import scala.annotation.tailrec
import scala.util.Try

trait DecemberFiveParser extends AdventOfCodeInstructionsParser[ (Long, Long), List[(Long, Long)], Long, List[Long]]  {

   class LongTupleListParserImp(override val day: Int, override val test: Boolean) extends GenericTupleParser[Long,Long] {

     override def stringToA(s: String): Long = s.toLong

     override def stringToB(s: String): Long = s.toLong

     override def lineParser(line: String): Option[(Long, Long)] = {
       val list = line.split("-").toList
       listToTuple(list)
     }
   }

   class LongParser(override val day: Int, override val test: Boolean) extends SingleValueParser[Long] {

     override def parseValue(line: String): Option[Long] = Try(line.toLong).toOption
   }

  override def inputParser: AdventOfCodeParser[(Long, Long), List[(Long, Long)]] = new LongTupleListParserImp(day, test)
  override def instructionParser: AdventOfCodeParser[Long, List[Long]] = new LongParser(day, test)
}

trait DecemberFive[A] extends DecemberTwentyFive[A, (List[(Long, Long)], List[Long]), Long] with DecemberFiveParser {

  implicit class RichLongTuple(longTuple: (Long, Long)) {
     def contains(i: Long): Boolean = i >= longTuple.left && i <= longTuple.right
     def contains(other: (Long, Long)): Boolean = longTuple.left <= other.left && longTuple.right >= other.right
     def adjacent(other: (Long, Long)): Boolean = other.left - longTuple.right == 1
     def rangeSize: Long = (longTuple.right - longTuple.left) + 1
  }

  override def day: Int = 5

  val (ranges, ingredients) = rawInput
}

trait DecemberFivePartOne extends DecemberFive[Int] {
  override def rawSolution: List[Long] = ingredients.filter{ ingredient => isFresh( ranges, ingredient) }

  override def solver: Solver[Long, Int] = listSizeSolver

  @tailrec
  private def isFresh(ranges: List[(Long, Long)], ingredient: Long) : Boolean = {0
    ranges match {
      case Nil => false
      case head :: tail =>
        head.contains(ingredient) || isFresh(tail, ingredient)
    }
  }
}

object DecemberFivePartOneTest extends DecemberFivePartOne with PuzzleTest
object DecemberFivePartOneSolution extends DecemberFivePartOne with PuzzleSolution

trait DecemberFivePartTwo extends DecemberFive[Long]  {

  override def solver: Solver[Long, Long] = listTotalSolver(0L)

  override def rawSolution: List[Long] = {
    val sortedRanges = ranges.sortBy{ case(start, _) => start}
    mergeRanges(sortedRanges)
      .map{ mergedRange => mergedRange.rangeSize }
  }

  private def mergeRanges(sortedRanges: List[(Long, Long)], acc: List[(Long, Long)] = List.empty): List[(Long, Long)] = {
    sortedRanges match {
      case Nil => acc
      case head :: tail =>
        if(acc.isEmpty) mergeRanges(tail, head :: acc)
        else {
          val accHead = acc.head
          val nextAcc = if ( accHead.contains(head) )  acc
            else if ( accHead.contains(head.left) || accHead.adjacent(head) )  (accHead.left, head.right) :: acc.tail
            else  head :: acc
          mergeRanges(tail, nextAcc)
        }
    }
 }
}

object DecemberFivePartTwoTest extends DecemberFivePartTwo with PuzzleTest
object DecemberFivePartTwoSolution extends DecemberFivePartTwo with PuzzleSolution