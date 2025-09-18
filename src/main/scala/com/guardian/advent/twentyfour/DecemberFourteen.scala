package com.guardian.advent.twentyfour

import com.guardian.advent.{AdventOfCodeParser, Grid, GridEntry}

import scala.collection.AbstractSeq
import scala.util.Try

case class Robot(id: Int, velocityX: Int, velocityY: Int)

case class RobotGridEntry(override val xPosition: Int, override val yPosition: Int, override val value: Char, robots: Set[Robot] = Set.empty) extends GridEntry[Char] {
  def addRobot(robot: Robot): RobotGridEntry = this.copy( robots = robots + robot )
  def takeRobot(robot: Robot): RobotGridEntry = (this.copy(robots = robots.filterNot( r => r == robot ) ))
  def isEmpty: Boolean = robots.isEmpty

  override def toString: String = {
    if (robots.isEmpty) "."
    else s"${robots.size}"
  }
}

trait DecemberFourteenParser extends AdventOfCodeParser[(Int, Int, Int, Int), List[(Int, Int, Int, Int)]] {
  override def sequenceToCollection(seq: Seq[(Int, Int, Int, Int)]): List[(Int, Int, Int, Int)] = seq.toList

  override def lineParser(line: String): Option[(Int, Int, Int, Int)] = {
    Try {
      line match {
        case s"p=$xPos,$yPos v=$velocityX,$velocityY"  =>(xPos.toInt, yPos.toInt, velocityX.toInt,  velocityY.toInt)
      }
    }.toOption
  }
}

trait DecemberFourteen extends December[Int, List[(Int, Int, Int, Int)], Int] with DecemberFourteenParser {

  override def day: Int = 14
  protected def gridWidth: Int = 11
  protected def gridHeight: Int = 7

  val robotMap = rawInput
    .zipWithIndex
    .map { case(rawRobot, index  ) =>
      val (startX, staryY, velocityX, velocityY) = rawRobot
      ((startX, staryY), Robot(index, velocityX, velocityY))
    }.toMap
7
   val gridEntries = (for{
     yPos: Int <- (0 until 11).toList
     xPos: Int <- (0 until 3).toList
   } yield (xPos, yPos))/*{
      val key = (xPos, yPos)
      val maybeRobot = robotMap.get(key)
      println(key)
      println(maybeRobot)
      val robots = maybeRobot match {
        case Some(r) => Set(r)
        case None => Set[Robot]()
      }
      RobotGridEntry(xPos, yPos, '-', robots)
   })*/

   //val grid = CharGrid(gridEntries.toSet)

  override def solver: Solver[Int, Int] = ???
  override def rawSolution: List[Int] = ???

}

trait DecemberFourteenPartOne extends DecemberFourteen {
  override val gridWidth: Int = 11
  override val gridHeight: Int = 7
}


object DecemberFourteenPartOneTest extends DecemberFourteenPartOne with PuzzleTest with App {
println(robotMap)
println("---")
println(gridEntries)
//println(grid)
}