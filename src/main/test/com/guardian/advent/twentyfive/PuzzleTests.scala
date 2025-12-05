package com.guardian.advent.twentyfive

import com.guardian.advent.DecemberTwentyFive
import com.guardian.advent.grid.CharGrid
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
    this.run()
  }

  "day two" should "return the correct values" in new TestRunner[Long, List[(Long, Long)], Long] {
    override def testsAndExpected: List[(DecemberTwo, Long)] = List(
      (DecemberTwoPartOneTest, 1227775554L),
      (DecemberTwoPartOneSolution, 19128774598L),
      (DecemberTwoPartTwoTest, 4174379265L),
      (DecemberTwoPartTwoSolution, 21932258645L),
    )
    this.run()
  }

  "day three" should "return the correct values" in  {

      val partOne = new TestRunner[Int, List[List[Int]], Int] {
        override def testsAndExpected: List[(DecemberTwentyFive[Int, List[List[Int]], Int], Int)] = List(
          (DecemberThreePartOneTest, 357),
          (DecemberThreePartOneSolution, 16812)
        )
      }

     val partTwo = new TestRunner[Long, List[List[Int]], Long] {
      override def testsAndExpected: List[(DecemberTwentyFive[Long, List[List[Int]], Long], Long)] = List(
        (DecemberThreePartTwoTest, 3121910778619L),
        (DecemberThreePartTwoSolution, 166345822896410L),
      )
     }
    partOne.run()
    partTwo.run()
  }

  "Day 4" should "return the correct values" in new TestRunner[Int, CharGrid, Int] {
    override def testsAndExpected: List[(DecemberTwentyFive[Int, CharGrid, Int], Int)] = List(
      (DecemberFourPartOneTest, 13),
      (DecemberFourPartOneSolution, 1480),
       (DecemberFourPartTwoTest, 43),
       (DecemberFourPartTwoSolution, 8899)
    )

    this.run()
  }

  trait TestRunner[A, B, C] {

    def testsAndExpected: List[(DecemberTwentyFive[A,B,C], A)]

    def run(): Unit = {
       testsAndExpected.foreach{
         case (puzzle, expectedSolution) => puzzle.solve shouldBe expectedSolution
      }
    }
  }
}
