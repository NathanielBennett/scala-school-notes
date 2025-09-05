package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.StringParser

import scala.util.Try

trait DecemberEleven extends December[Long, List[String], Long] with StringParser {

  override def day: Int = 11

  override def solver: Solver[Long, Long] = new Solver[Long, Long] {

    override def solution(list: List[Long]): Long = list.headOption.getOrElse(0L)
  }

  def flipLimit: Int = 25

  val stones = rawInput.mkString.split(" ").toList.flatMap {
    stoneString => Try(stoneString.toLong).toOption
  }

  def nextStones(stone: Long): List[Long] = {
    stone match {
      case 0L => List(1L)
      case stone =>
        val stoneString = stone.toString
        val stoneStringLength = stoneString.length
        if(stoneStringLength % 2 == 0) {
          stoneString.splitAt(stoneStringLength / 2) match {
            case (stoneOne, stoneTwo) => List(stoneOne.toLong, stoneTwo.toLong)
          }
        }
        else List(stone * 2024)
    }
  }

  def flip(currentStones: List[Long], flipCnt: Int = 0): Long

  override def rawSolution: List[Long] = List(flip(stones))
}

trait DecemberElevenPartOne extends DecemberEleven {

  def flipStones(currentStones: List[Long]): List[Long] = {
    currentStones.flatMap{ stone  => nextStones(stone) }
  }

  override def flip(currentStones: List[Long], flipCnt: Int = 0): Long = {
   // println(s"F: $flipCnt ${currentStones.size}")
    if(flipCnt == flipLimit) currentStones.size
    else {
      val nextStones = flipStones(currentStones)
      flip(nextStones, flipCnt + 1)
    }
  }
}

object DecemberElevenPartOneTest extends DecemberElevenPartOne with PuzzleTest
object DecemberElevenPartOneSolution extends DecemberElevenPartOne with PuzzleSolution


object DecemberElevenPartTwoSolution extends DecemberEleven with PuzzleSolution with App {

  val base = stones.map{ case i => (i -> 1L)}.toMap

  def iterateCnt(stoneAndCounts: Map[Long, Long]): Map[Long, Long] = {
    (for {
      (stone, count) <- stoneAndCounts.iterator
      nextStone <- nextStones(stone)
    } yield nextStone -> count).groupMapReduce{ case (stone, _ ) => stone }{ case(_, cnt) => cnt }{ case(a, b) => a + b }
  }

  def findDupes(current: Map[Long, Long], cnt: Int = 0): Map[Long, Long] = {
    if( cnt == 75 ) current
    else findDupes(iterateCnt(current), cnt + 1)
  }

  override def flip(currentStones: List[Long], flipCnt: Int): Long = {

    val baseCounts = currentStones.map{ stone => (stone -> 1L)}.toMap
    findDupes(baseCounts)
      .foldLeft(0L) {
        case (total, (_, frequency)) => total + frequency
      }
  }
}


