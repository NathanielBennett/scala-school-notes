package com.guardian.advent.twentyfive

import com.guardian.advent.DecemberTwentyFive
import com.guardian.advent.parsers.IntegerListParser
import com.guardian.advent.twentyfour.{PuzzleSolution, PuzzleTest, Solver}

import scala.util.Try

trait DecemberThree[A] extends DecemberTwentyFive[A, List[List[Int]], A] with IntegerListParser {

  override def day: Int = 3

  override def rawSolution: List[A] = rawInput.map { bank => highVoltage(bank) }

  def highVoltage(ints: List[Int]): A
}

trait DecemberThreePartOne extends DecemberThree[Int] {

  override def solver: Solver[Int, Int] = listTotalSolver(0)

  override def highVoltage(batteries: List[Int]): Int = {

    lazy val lastBattery = batteries.size - 1
    val maxVoltage = batteries.max
    val maxVoltageIndex = batteries.indexOf(maxVoltage)
    val (leftBattery, rightBattery) = if(maxVoltageIndex == lastBattery) {
      (batteries.reverse.tail.max, maxVoltage)
    } else {
      (
       maxVoltage,
       batteries.slice(maxVoltageIndex + 1, batteries.size).max
      )
    }
    s"$leftBattery$rightBattery".toInt
  }
}

object DecemberThreePartOneTest extends DecemberThreePartOne with PuzzleTest
object DecemberThreePartOneSolution extends DecemberThreePartOne with PuzzleSolution

trait DecemberThreePartTwo extends DecemberThree[Long] {

  override def solver: Solver[Long, Long] = listTotalSolver(0L)
  override def highVoltage(ints: List[Int]): Long = buildBank(ints.zipWithIndex)

  private def buildBank(rawIndexedBank: List[(Int, Int)], currSlots: Int = 11, currSliceStart: Int = 0, acc: List[Int] = List.empty): Long = {
    if (acc.size == 12) acc.reverse.mkString.toLong
    else {
      val slice = rawIndexedBank.slice(currSliceStart, rawIndexedBank.length - currSlots)
      val (max, maxIndex) = slice.maxBy(_._1)
      buildBank(rawIndexedBank, currSlots - 1, maxIndex + 1, max :: acc)
    }
  }
}

object DecemberThreePartTwoTest extends DecemberThreePartTwo with PuzzleTest
object DecemberThreePartTwoSolution extends DecemberThreePartTwo with PuzzleSolution
