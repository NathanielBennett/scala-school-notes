package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeParser, GridEntry}

import scala.util.Try

case class Robot( initialPosition: (Int, Int), velocityX: Int, velocityY: Int)

sealed case class RobotGridEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char, robots: Set[Robot]) extends GridEntry[Char] {
  def addRobot(robot: Robot): RobotGridEntry = this.copy( robots = robots + robot )
  def takeRobot(robot: Robot): RobotGridEntry = (this.copy(robots = robots.filterNot( r => r == robot ) ))
  def isEmpty: Boolean = robots.isEmpty
}

trait DecemberFourteenParser extends AdventOfCodeParser[Robot, List[Robot]] {
  override def lineParser(line: String): Option[Robot] = {
    Try {
      line match {
        case s"p=$xPos,$yPos v=$velocityX,$velocityY"  =>
          Robot(initialPosition = (xPos.toInt, yPos.toInt), velocityX = velocityX.toInt, velocityY = velocityY.toInt)
      }
    }.toOption
  }
}

trait DecemberFourteen extends December[Int, List[Robot], Int] with DecemberFourteenParser {

  override def day: Int = 14
  val rawERobots = rawInput
}
