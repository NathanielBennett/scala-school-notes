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

    val testAndExpected: List[(December[Int, CharGrid, GridEntry[Char]] , Int)] = List(

      (DecemberSixPartOneTest, 41),
      (DecemberSixPartOneSolution, 4890),
      (DecemberSixRefactorPartTwoTest, 6),
     // (DecemberSixRefactorPartTwoSolution, 1995),
    )

     testAndExpected.foreach {
      case (puzzle, expectedResult ) => puzzle.solve should be(expectedResult)
    }
  }

  "Day Seven" should "return the correct values" in {
    val testAndExpected: List[(December[Long, List[(Long, List[Long])], Long], Long)] = List (
      (DecemberSevenPartOneTest, 3749L),
      (DecemberSevenPartOneSolution, 6231007345478L),
      (DecemberSevenPartTwoTest, 11387L),
      (DecemberSevenPartTwoSolution, 333027885676693L)
    )

    testAndExpected.foreach {
      case (puzzle, expectedResult ) => puzzle.solve should be(expectedResult)
    }
  }

  "Day Eight" should "return the correct values" in {
    val testAndExpected: List[(December[Int, CharGrid, AntennaGridEntry], Int)] = List (
      (DecemberEightPartOneTest, 14),
      (DecemberEightPartTwoTest, 34),
      (DecemberEightPartOneSolution, 398),
      (DecemberEightPartTwoSolution, 1333)
    )

    testAndExpected.foreach {
      case (puzzle, expectedResult) => puzzle.solve shouldBe(expectedResult)
    }
  }

  "Day Nine" should "return the correct values" in {
    val testAndExpected: List[(December[Long, List[String], (Int, Int)], Long)] = List(
      (DecemberNinePartOneTest, 1928),
      //(DecemberNinePartOneSolution, 6341711060162L)
      (DecemberNinePartTwoTest, 2858L),
      (DecemberNinePartTwoSolution, 6377400869326L),
    )
    testAndExpected.foreach {
      case (puzzle, expectedResult) => puzzle.solve shouldBe(expectedResult)
    }
  }

  "Day 10" should "return the correct values" in {
    val textAndExpected: List[(December[Int, IntGrid, Int], Int)] = List(
      (DecemberTenPartOneTest, 36),
      (DecemberTenPartOneSolution, 552),
      (DecemberTenPartTwoTest, 81),
      (DecemberTenPartTwoSolution, 1225),

    )

    textAndExpected.foreach {
      case(puzzleTest, expectedResult) => puzzleTest.solve shouldBe (expectedResult)
    }
  }

  "Day 11" should "return the correct values" in {
    val testAndExpected: List[(December[Long, List[String], Long], Long)] = List(
      (DecemberElevenPartOneTest, 55312L),
      (DecemberElevenPartOneSolution, 194482L),
      (DecemberElevenPartTwoSolution, 232454623677743L),
    )

    testAndExpected.foreach {
      case(puzzleTest, expectedResult) => puzzleTest.solve shouldBe (expectedResult)
    }
  }
}