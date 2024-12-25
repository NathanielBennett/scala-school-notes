package com.guardian.advent.twentyfour

import org.scalatest._
import flatspec._
import matchers._

class ExerciseTests extends AnyFlatSpec with should.Matchers {

  "Day one" should "return the correct values" in {
       val testsAndExpected: List[(DecemberOne, Int)] = List(
         (DecemberOnePartOneTest, 11),
         (DecemberOnePartOneSolution, 1830467),
         (DecemberOnePartTwoTest, 31),
         (DecemberOnePartTwoSolution, 26674158)
       )

       testsAndExpected.foreach {
         case (adventPuzzle, expectedResult) => adventPuzzle.solve() should be(expectedResult)
       }
    }

  "Day two" should "return the correct values" in {
       val testsAndExpected: List[(DecemberTwo, Int)] = List(
         (DecemberTwoPartOneTest, 2),
         (DecemberTwoPartOneSolution, 402),
         (DecemberTwoPartTwoTest, 4),
         (DecemberTwoPartTwoSolution, 455)
       )

       testsAndExpected.foreach {
         case (adventPuzzle, expectedResult) =>
           println(adventPuzzle.getClass)
           adventPuzzle.solve() should be(expectedResult)
       }
    }

  "Day three" should "return the correct values" in {
      DecemberThreePartOneTest.solve() should be (161)
      DecemberThreePartOneSolution.solve() should be (155955228L)
      DecemberThreePartTwoTest.solve() should be (48L)
      DecemberThreePartTwoSolution.solve() should be (100189366)
  }

  "Day four" should "return the correct values" in {
     val testsAndExpected: List[(DecemberFour, Int)] = List(
       (DecemberFourPartOneTest, 18),
       (DecemberFourPartOneSolution, 2434),
       (DecemberFourPartTwoTest, 9),
       (DecemberFourPartTwoSolution, 1835)
     )
    testsAndExpected.foreach {
      case (adventPuzzle, expectedResult) =>
        println(adventPuzzle.getClass)
        adventPuzzle.solve() should be(expectedResult)
    }

  }

  "Day five" should "return the correct valuws" in {
      DecemberFivePartOneTest.solve() should be(143)
      DecemberFivePartOneSolution.solve() should be(5091)
  }
}