package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeParser, MultiLineParser}
import scala.util.Try


case class ClawMachine(buttonA: (Int, Int), buttonB: (Int, Int), prize: (Long, Long) )

trait DecemberThirteenParser extends MultiLineParser[ClawMachine, List[ClawMachine]] {

  override def sequenceToCollection(seq: Seq[ClawMachine]): List[ClawMachine] = seq.toList

  protected def prizePosition(s: String): Long = s.toLong

  def parseButton(s: String): (Int, Int) = s match { case s"Button $_: X+$x, Y+$y" => (x.toInt, y.toInt) }
  def parsePrize(s: String): (Long, Long) = s match { case s"Prize: X=$x, Y=$y" => (prizePosition(x), prizePosition(y)) }

  override def rawInput: List[ClawMachine] = {

    val rawLines = getLines()
    groupRawLines(rawLines).flatMap { roughObject =>
       Try {
          val buttonA = parseButton(roughObject(2))
          val buttonB = parseButton(roughObject(1))
          val prize = parsePrize(roughObject(0))
          ClawMachine(buttonA, buttonB, prize)
       }.toOption
    }
  }
}

trait DecemberThirteen extends December[Long, List[ClawMachine], Long] with DecemberThirteenParser {
  override def day: Int = 13

  override def solver: Solver[Long, Long] = listTotalSolver(0L, test)

  protected def inRange(long: Long): Option[Long] = if(long >= 0L && long <= 100L) Some(long) else None

  private def devideByZero(a : Long, b: Long): Option[Long] = if (a % b == 0) Some(a / b) else None

  def findIntersection(clawMachine: ClawMachine): Option[(Long, Long)] = {
    val (buttonAx, buttonAy) = clawMachine.buttonA
    val (buttonBx, buttonBy) = clawMachine.buttonB
    val (prizeX, prizeY) = clawMachine.prize

    val axByBy = buttonAx * buttonBy
    val prizeXbyBy = prizeX * buttonBy

    val ayByBx = buttonAy * buttonBx
    val prizeYbyBx = prizeY * buttonBx

    for {
      maybeA <- devideByZero(prizeXbyBy - prizeYbyBx, axByBy - ayByBx )
      maybeB <- devideByZero(prizeY - buttonAy * maybeA, buttonBy)
      maybeApress <- inRange(maybeA)
      maybeBpress <- inRange(maybeB)
    } yield (maybeApress, maybeBpress)
  }

  override def rawSolution: List[Long] = {
    rawInput.flatMap { clawMachine =>
        findIntersection(clawMachine).map {
          case(buttonA, buttonB) =>
            buttonA * 3 + buttonB
        }
    }
  }
}

object DecemberThirteenPartOneTest extends DecemberThirteen with PuzzleTest
object DecemberThirteenPartOneSolution extends DecemberThirteen with PuzzleSolution

trait DecemberThirteenPartTwo extends DecemberThirteen {
  override def prizePosition(s: String): Long = s.toLong + 10000000000000L
  override def inRange(long: Long): Option[Long] = Some(long)
}

object DecemberThirteenPartTwoTest extends DecemberThirteenPartTwo with PuzzleTest
object DecemberThirteenPartTwsSolution extends DecemberThirteenPartTwo with PuzzleSolution