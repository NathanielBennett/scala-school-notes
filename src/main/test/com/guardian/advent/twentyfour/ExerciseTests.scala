package com.guardian.advent.twentyfour

import org.scalatest._
import flatspec._
import matchers._

class ExerciseTests extends with AnyFlatSpec with should.Matchers {

  "Day one" should "return the correct values" in {
      2 + 2 should be (4)
    }
}