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

  protected def validListConCat(target: Long, remaining: List[Long], acc: LazyList[Long] = LazyList.empty): Boolean = {
    println(s"Validate $remaining")
    remaining match {
      case Nil => false
      case head :: tail =>
        println(s"Check $target -=-=- ${acc.length} : $head ")
        if (acc.isEmpty) validListConCat(target, tail, LazyList(head))
        else {
          val nextAcc = acc.flatMap { a => LazyList(a + head, a * head) ++ acc.map { b => s"$b$head".toLong } }.filter { l => l <= target }
          println("Mext")
          if (nextAcc.isEmpty) false
          else {
            println("check")
            if (tail.isEmpty) {
              println("empty")
              nextAcc.contains(target)
            }
            else {
              println("Recur")
              validListConCat(target, tail, nextAcc)
            }
          }
        }
    }
  }

  override def rawSolution: List[Long] = {
    val (valid, invalid) = input.partition{
      case (target, nums) => validList(target, nums)
    }

    println(s"Invalid: ${invalid.length}")
    val alternatveValid = invalid.filter {
      case(target, nums) =>
        println(s"Loop")
        val good = validListConCat(target, nums)
        println(s" ($good)")
        good
    }

    (valid ++ alternatveValid).map{ case(target, _) => target}

  }
}

object DecemberSevenPartTwoTest extends DecemberSeverPartTwo with PuzzleTest
object DecemberSevenPartTwoSolution extends DecemberSeverPartTwo with PuzzleSolution
