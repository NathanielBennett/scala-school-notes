package com.guardian.advent.twentyfour

import com.guardian.advent.{Direction, GridEntry}
import org.scalatest._
import flatspec._
import matchers._

class ExerciseTests extends AnyFlatSpec with should.Matchers {

  "Day one part one" should "return the correct values" in {
     val testsAndExpectedPartOne: List[(DecemberOne[(Int, Int)], Int)] = List(
        (DecemberOnePartOneTest, 11),
        (DecemberOnePartOneSolution, 1830467)
     )

    testsAndExpectedPartOne.foreach {
      case (adventPuzzle, expectedResult) => adventPuzzle.solve should be(expectedResult)
    }

    val testsAndExpectedPartTwo: List[(DecemberOne[Int], Int)] = List(
        (DecemberOnePartTwoTest, 31),
        (DecemberOnePartTwoSolution, 26674158)
    )

    testsAndExpectedPartTwo.foreach {
      case (adventPuzzle, expectedResult) => adventPuzzle.solve should be(expectedResult)
    }

  }


  "Day one part two" should "return the correct values" in {
       val testsAndExpected: List[(DecemberOne[Int], Int)] = List(
         (DecemberOnePartTwoTest, 31),
         (DecemberOnePartTwoSolution, 26674158)
       )

       testsAndExpected.foreach {
         case (adventPuzzle, expectedResult) => adventPuzzle.solve should be(expectedResult)
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
           adventPuzzle.solve should be(expectedResult)
       }
    }

  "Day three" should "return the correct values" in {
      DecemberThreePartOneTest.solve should be (161)
      DecemberThreePartOneSolution.solve should be (155955228L)
      DecemberThreePartTwoTest.solve should be (48L)
      DecemberThreePartTwoSolution.solve should be (100189366)
  }

  "Day four part 1" should "return the correct values" in {
     val testsAndExpected: List[(DecemberFourPartOne, Int)] = List(
       (DecemberFourPartOneTest, 18),
       (DecemberFourPartOneSolution, 2434)
     )
     testsAndExpected.foreach {
       case (adventPuzzle, expectedResult) =>
         adventPuzzle.solve should be (expectedResult)
     }
  }

  "Day 4 Part 2" should "return the correct values" in {
    val testsAndExpected: List[(DecemberFourPartTwo, Int)] = List(
         (DecemberFourPartTwoTest, 9),
         (DecemberFourPartTwoSolution, 1835)
     )

    testsAndExpected.foreach {
      case (adventPuzzle, expectedResult) =>
        println(adventPuzzle.getClass)
        adventPuzzle.solve should be(expectedResult)
    }
  }

  "Day five" should "return the correct valuws" in {
      DecemberFiveSecondPartOneTest.solve should be(143)
      DecemberFiveSecondPartOneSolution.solve should be(5091)
      DecemberFivePartTwoTest.solve should be (123)
      DecemberFivePartTwoSolution.solve should be (4681)
  }

  "Day six" should "return the correct values" in {
    val testsAndExpected: List[(DecemberSix, Int)] = List(
     // (DecemberSixPartOneTest, 41),
     // (DecemberSixPartOneSolution, 4890),
       (DecemberSixPartTwoTest, 6),
     // (DecemberSixPartTwoSolution, 6)
    )

    testsAndExpected.foreach {
      case (adventPuzzle, expectedResult) =>
        println(adventPuzzle.getClass)
        adventPuzzle.solve should be(expectedResult)
    }
  }
}