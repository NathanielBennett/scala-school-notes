package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeParser

import scala.util.Try

case class Robot(id: Int, velocityX: Int, velocityY: Int)

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

  protected val gridWidth: Int = 11
  protected val gridHeight: Int = 7
  final val moveLimit = 100

  override def solver: Solver[Int, Int] = new Solver[Int, Int] {

    override def solution(list: List[Int]): Int = list.foldLeft(1) {
      case (total, robots) => total * robots
    }
  }

  lazy val robotMap: List[((Int, Int), Robot)] =
    rawInput
      .zipWithIndex
      .map { case (rawRobot, index) =>
        val (startX, staryY, velocityX, velocityY) = rawRobot
        ((startX, staryY), Robot(index, velocityX, velocityY))
      }

  def modoloOrAdd(start: Int, increment: Int, limit: Int): Int = {
    val sum = start + increment
    if (sum >= 0) sum % limit
    else sum + limit
  }

  def getNextPos(currPos: (Int, Int), robot: Robot): (Int, Int) = {
    val (currentX, currentY) = currPos
    (
      modoloOrAdd(currentX, robot.velocityX, gridWidth),
      modoloOrAdd(currentY, robot.velocityY, gridHeight)
    )
  }

  def endPosition(currPos: (Int, Int), robot: Robot, mvCnt: Int = 0): (Int, Int) = {
    if (mvCnt == moveLimit) currPos
    else {
      val nextPos = getNextPos(currPos, robot)
      endPosition(nextPos, robot, mvCnt + 1)
    }
  }

  def positionRobots(robotsAndStarts: List[((Int, Int), Robot)])(nextPosition: ((Int, Int), Robot) => (Int, Int)): List[((Int, Int), Robot)] = {
    robotsAndStarts.map {
      case (start, robot) =>
        val end = nextPosition(start, robot)
        (end, robot)
    }
  }

  def groupRobots(robotsAndPositions: List[((Int, Int), Robot)]): List[((Int, Int), List[((Int, Int), Robot)])] =
    robotsAndPositions
      .groupBy { case (pos, _) => pos }
      .toList
}

trait DecemberFourteenPartOne extends DecemberFourteen {

  def groupRobotsByEndPosition(robotsAndStarts: List[((Int, Int), Robot)]): List[((Int, Int), List[Robot])] = {
    val endPositions = positionRobots(robotsAndStarts) {case (pos, robot) => endPosition(pos, robot) }
    groupRobots(endPositions)
      .map{ case(pos, robots) =>
        (
          pos,
          robots.map{ case(_, robot) => robot}
        )
      }
  }

  override def rawSolution: List[Int] = {

    val xQuadrant = gridWidth / 2
    val yQuadrant = gridHeight / 2

    val groupedRobots = groupRobotsByEndPosition(robotMap)

    List(
      ((0, xQuadrant), (0, yQuadrant)),
      ((0, xQuadrant), (yQuadrant + 1, gridHeight)),
      ((xQuadrant + 1, gridWidth), (0, yQuadrant)),
      ((xQuadrant + 1, gridWidth), (yQuadrant + 1, gridHeight))
    ).map {
      case ((minX, maxX), (minY, maxY) ) =>
        groupedRobots.filter{ case((x, y), _) => x >= minX && x < maxX && y >= minY && y < maxY }
          .foldLeft(0) {
            case (robotsForQuadrant, robotsForPos) =>
              val (_, robots) = robotsForPos
              robotsForQuadrant + robots.size
          }
    }
  }
}
object DecemberFourteenPartOneTest extends DecemberFourteenPartOne with PuzzleTest {
  override val gridWidth: Int = 11
  override val gridHeight: Int = 7
}

object DecemberFourteenPartOneSolution extends DecemberFourteenPartOne with PuzzleSolution {
  override val gridWidth: Int = 101
  override val gridHeight: Int = 103
}

trait DecemberFourteenPartTwo extends DecemberFourteen {

  private def findTree(robotPosList: List[((Int, Int), Robot)], mvCnt: Int = 0): Int = {
    val grouped = groupRobots(robotPosList)
    if(robotPosList.size == grouped.size) mvCnt
    else {
      val nextRobotPos = positionRobots(robotPosList)(getNextPos)
      findTree(nextRobotPos, mvCnt + 1)
    }
  }

  override def rawSolution: List[Int] = {
    val cnt = findTree(robotMap)
    List(cnt)
  }
}

object DecemberFourteenPartTwoSolution extends DecemberFourteenPartTwo with PuzzleSolution {
  override val gridWidth: Int = 101
  override val gridHeight: Int = 103
}