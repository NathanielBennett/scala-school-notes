 package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeInstructionsParser, December}
import com.guardian.advent.parsers.{IntegerListParser, IntegerTupleParser}

import scala.collection.{LinearSeq, immutable}

trait DecemberFiveParser extends AdventOfCodeInstructionsParser[ (Int, Int), List[(Int, Int)], List[Int], List[List[Int]] ] {

  type Rule = (Int, Int)

  class IntegerTupleParserImp(override val day: Int, override val test: Boolean) extends IntegerTupleParser {
    override def lineParser(line: String): Option[Rule] = {
      val l = line.toStringList('|')
      listToTuple(l)
    }
  }

  class IntegerListParserImp(override val day: Int, override val test: Boolean) extends IntegerListParser {
    override def lineParser(line: String): Option[List[Int]] = {
      val l = line.toStringList(',')
      listToIntList(l)
    }
  }

  override def inputParser = new IntegerTupleParserImp(day, test)
  override def instructionParser = new IntegerListParserImp(day, test)
}

trait DecemberFive extends December[Int, (List[(Int, Int)], List[List[Int]]), Int] with DecemberFiveParser {

  implicit def linearSeqToList[A](linearSeq: LinearSeq[A]): List[A] = linearSeq.toList

  override def solver: Solver[Int, Int] = listTotalSolver(0, test)

  override def day = 5

  val (rules, pageUpdates) = rawInput


  protected def middle(list: List[Int], last: Option[Int] = None, cnt: Int = 0 ): Option[Int] = {
    list.headOption.flatMap {
      head =>
        if (cnt > list.size) last
        else middle(list.tail, Some(head), cnt + 1)
    }
  }

  protected def pageOrder(pages: List[Int]): List[Int] = {

    val rulesMap = rules.filter{ case(left, right) => pages.contains(left) && pages.contains(right) }.listMapRight
    pages.map {
      page =>
        val rulesForPage = rulesMap.get(page).map{ls => ls.toList}.getOrElse(List.empty)
        (page, rulesForPage)
    }
    .sortBy{ case(_, followingPages) => followingPages.length }
    .map{ case(page, _) => page}
  }
}

trait DecemberFivePartOne extends DecemberFive {

  override def rawSolution = pageUpdates.filter{ pageUpdate => pageUpdate == pageOrder(pageUpdate) }
    .flatMap { correctPages => middle(correctPages) }
}

trait DecemberFivePartTwo extends DecemberFive {

   override def rawSolution = pageUpdates.flatMap {
     pageUpdate =>
       val orderedPage = pageOrder(pageUpdate)
       if (orderedPage == pageUpdate) None
       else {
         val ordered = orderedPage
           .zipWithIndex.toList.sortBy{ case(_, index) => index}
           .map{case (page, _) => page}
         Some(ordered)
       }
   }
   .flatMap { ordered => middle(ordered) }

}

object DecemberFiveSecondPartOneTest extends DecemberFivePartOne with PuzzleTest
object DecemberFiveSecondPartOneSolution extends DecemberFivePartOne with PuzzleSolution

object DecemberFivePartTwoTest extends DecemberFivePartTwo with PuzzleTest
object DecemberFivePartTwoSolution extends DecemberFivePartTwo with PuzzleSolution



