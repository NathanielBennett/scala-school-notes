package com.guardian.advent.twentyfive

import com.guardian.advent.DecemberTwentyFive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._



class PuzzleTests extends AnyFlatSpec with should.Matchers {

  "day one" should "return the correct values" in new TestRunner[Int, List[(String, Int)], Int] {
    override def testsAndExpected: List[(DecemberOne, Int)] = List(
      (DecemberOnePartOneTest, 3),
      (DecemberOnePartOneSolution, 1165),
      (DecemberOnePartTwoTest, 6),
      (DecemberOnePartTwoSolution, 6496),
    )
  }

  "day two" should "return the correct values" in new TestRunner[Long, List[(Long, Long)], Long] {
    override def testsAndExpected: List[(DecemberTwo, Long)] = List(
      (DecemberTwoPartOneTest, 1227775554L),
      (DecemberTwoPartOneSolution, 19128774598L),
      (DecemberTwoPartTwoTest, 4174379265L),
      (DecemberTwoPartTwoSolution, 4174379265L),
    )
  }

  trait TestRunner[A, B, C] {

    def testsAndExpected: List[(DecemberTwentyFive[A,B,C], A)]

    testsAndExpected.foreach{
      case (puzzle, expectedSolution) => puzzle.solve shouldBe expectedSolution
    }
  }
}
