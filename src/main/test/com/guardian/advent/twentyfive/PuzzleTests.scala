package com.guardian.advent.twentyfive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._



class PuzzleTests extends AnyFlatSpec with should.Matchers {

  "day one" should "return the correct values" in {
    val testsAndExpected: List[(DecemberOne, Int)] = List(
      (DecemberOnePartOneTest, 3),
      (DecemberOnePartOneSolution, 1165),
      (DecemberOnePartTwoTest, 6),
      (DecemberOnePartTwoSolution, 6496),
    )

    testsAndExpected.foreach{
      case (puzzle, expectedSolution) => puzzle.solve shouldBe expectedSolution
    }
  }
}
