package com.guardian.advent.twentyfive

import com.guardian.advent.DecemberTwentyFive
import com.guardian.advent.parsers.GenericTupleParser
import com.guardian.advent.twentyfour.{PuzzleSolution, PuzzleTest, Solver}

import scala.annotation.tailrec

trait DecemberOneParser extends GenericTupleParser[String, Int] {
  override def stringToA(s: String): String = s
  override def stringToB(s: String): Int = s.toInt

  override def lineParser(line: String): Option[(String, Int)] = {
    val list = List(line.take(1), line.tail)
    listToTuple(list)
  }
}

trait DecemberOne extends DecemberTwentyFive[Int, List[(String, Int)], Int] with DecemberOneParser {

  override def day: Int = 1

  def filterList(item: Int): Boolean = true

  def nextDialPoint(instruction: (String, Int), current: Int): Int = {
    val (direction, turn) = instruction
    val nextRawTurn = if (direction.equals("R")) current + turn else current - turn
    val moded = nextRawTurn % 100
    if (moded >= 0) moded else 100 + moded
  }

  def nextAcc(instruction: (String, Int), current: Int): (Int, Int)

  @tailrec
  final def shiftDial(rotations: List[(String, Int)], current: Int = 50, acc: List[Int] = List.empty): List[Int] = {
    rotations match {
      case Nil => acc
      case head :: tail =>
       val (nextCurrent, nextTurn) = nextAcc(head, current)
       shiftDial(tail, nextCurrent, nextTurn :: acc)
    }
  }

  override def rawSolution: List[Int] = shiftDial(rawInput).filter { i => filterList(i) }
}

trait DecemberOnePartOne extends DecemberOne {
  override def solver: Solver[Int, Int] = listSizeSolver

  override def filterList(item: Int): Boolean = item == 0

  override def nextAcc(instruction: (String, Int), current: Int): (Int, Int) = {
    val next = nextDialPoint(instruction, current)
    (next, next)
  }
}

object DecemberOnePartOneTest extends DecemberOnePartOne with PuzzleTest
object DecemberOnePartOneSolution extends DecemberOnePartOne with PuzzleSolution

trait DecemberOnePartTwo extends DecemberOne {

  override def solver: Solver[Int, Int] = listTotalSolver(0)
  override def nextAcc(instruction: (String, Int), current: Int): (Int, Int)= {

    lazy val (direction, turn) = instruction

    val nextCurrent  = nextDialPoint(instruction, current)
    val zeroCount = if(direction.equals("R")) (current + turn) / 100
        else {
          val raw = current - turn
          if (raw > 0) 0
          else {
            val abs = Math.abs(current - turn) / 100
            if (current == 0) abs else abs + 1
          }
        }
    (nextCurrent, zeroCount)
  }
}

object DecemberOnePartTwoTest extends DecemberOnePartTwo with PuzzleTest
object DecemberOnePartTwoSolution extends DecemberOnePartTwo with PuzzleSolution
