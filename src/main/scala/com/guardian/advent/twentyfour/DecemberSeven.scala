package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeParser

import scala.collection.{AbstractSeq, GenMap}
import scala.util.Try


trait DecemberSeverParser extends AdventOfCodeParser[(Long, List[Long]), List[(Long, List[Long])]] {

  val lineMatcher = """^(\d+):(.*)""".r

  override def lineParser(line: String): Option[(Long, List[Long])] = {

    def splitNums(line: String): Try[(Long, String)] = Try {
       val lineMatcher(target, nums) = line
      (target.toLong, nums.trim)
    }

    def parseLongs(nums: String): Try[List[Long]] = Try {
     nums.split(" ").map(_.toLong).toList
    }

    (for {
      (target, numberStrings) <- splitNums(line)
      numbers <- parseLongs(numberStrings)
    } yield (target, numbers)).toOption
  }

  override def sequenceToCollection(seq: Seq[(Long, List[Long])]): List[(Long, List[Long])] = seq.toList

  override def toSeq(list: List[String]): AbstractSeq[String] = list
}

trait DecemberSeven extends December[Long, List[(Long, List[Long])], Long] with DecemberSeverParser {

  override def day: Int = 7

  val input: List[(Long, List[Long])] = rawInput

  override def solver: Solver[Long, Long] = new ListTotalSolution[Long, Long] {
    override def foldSeed: Long = 0L
    override implicit val addable: Addable[Long, Long] = AddLong
  }

  protected def validList(target: Long, remaining: List[Long], acc: LazyList[Long] = LazyList.empty): Boolean = {
  //   if (acc.contains(target)) true
    remaining match {
      case Nil => false
      case head :: tail =>
        if (acc.isEmpty) validList(target, tail, LazyList(head))
        else {
          val nextAcc = acc.flatMap { a => List(a + head, a * head) }
          nextAcc.contains(target) || validList(target, tail, nextAcc)
        }
    }
  }
}

trait DecemberSeverPartOne extends DecemberSeven {

  override def rawSolution: List[Long] = {
    input.filter{ case(target, nums) =>
      val good = validList(target, nums)    //  println(s"$target: $nums ($good)")
      good
    }
    .map{ case(target, _) => target }
  }
}

object DecemberSevenPartOneTest extends DecemberSeverPartOne with PuzzleTest
object DecemberSevenPartOneSolution extends DecemberSeverPartOne with PuzzleSolution

trait DecemberSeverPartTwo extends DecemberSeven {

  protected def validListConCat(target: Long, remaining: List[Long], acc: List[Long] = List.empty): Boolean = {
    remaining match {
      case Nil => false
      case head :: tail =>
        if (acc.isEmpty) validListConCat(target, tail, List(head))
        else {
          val nextAcc = acc.flatMap { a => List(a + head, a * head) }  ++ acc.map { b => s"$b$head".toLong }.filter { l => l <= target }
          if (nextAcc.isEmpty) false
          else {
            if (tail.isEmpty) nextAcc.contains(target)
            else validListConCat(target, tail, nextAcc)
          }
        }
    }
  }

  override def rawSolution: List[Long] = {
    val (valid, invalid) = input.partition{
      case (target, nums) => validList(target, nums)
    }

    val alternatveValid = invalid.filter {
      case(target, nums) => validListConCat(target, nums)
    }

    (valid ++ alternatveValid).map{ case(target, _) => target}
  }
}

object DecemberSevenPartTwoTest extends DecemberSeverPartTwo with PuzzleTest
object DecemberSevenPartTwoSolution extends DecemberSeverPartTwo with PuzzleSolution with App {
  println(s"solve: $solve")
  //333027885676693
}
