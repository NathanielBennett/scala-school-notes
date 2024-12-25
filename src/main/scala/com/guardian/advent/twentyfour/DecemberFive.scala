package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCode

import scala.util.Try

trait DecemberFive extends AdventOfCode[Int] {

  override def day: Int = 5

  type Rule = (Int, Int)
  type RuleMatcher = (Rule, Rule) => Boolean

  protected def toTuples(others: List[Int], lastHead: Option[Int] = None, acc: List[(Int, Int)] = List.empty) : List[Rule] = {
    others match {
      case Nil => acc.reverse
      case head :: Nil => toTuples(Nil, Some(head), lastHead.map{ lh => (lh, head) :: acc}.getOrElse(acc))
      case head :: tail => toTuples(tail, Some(head), (head, tail.head) :: acc )
    }
  }

  protected def middle(list: List[Int], acc: List[Int] = List.empty ): Option[Int] = {
    list.headOption.flatMap {
      head =>
        if (acc.size > list.size) acc.headOption
        else middle(list.tail, head :: acc)
    }
  }


  private def listToTuple(list: List[String]): Option[Rule] = {
    list.map(_.trim) match {
      case head :: next :: _ => Try { (head.toInt, next.toInt ) }.toOption
      case _ => None
    }
  }

  private def listToIntList(list: List[String]): Option[List[Int]] = Try {
      list.map(_.trim).map{ s => s.toInt }
  }.toOption

  val (input, instructions) = instructionParser[Rule, List[Int]](test)(
     line => {
       val l = line.split('|').toList
       listToTuple(l)
     } ,
     line => {
       val l = line.split(",").toList
       listToIntList(l)
     }
  )

  val instuctionsTuples = instructions.map { instruction =>
     val tuples = toTuples(instruction)
    (instruction, tuples)
  }

  private def ruleEqualsLeft(rule: Rule, otherRule: Rule): Boolean = otherRule._1 == rule._1
  private def ruleEqualsRight(rule: Rule, otherRule: Rule): Boolean = otherRule._2 == rule._2

  private def checkRules(tuples: List[Rule], filter: RuleMatcher, ruleMatcher: RuleMatcher): Boolean = {
    tuples.isEmpty || {
      val rules = input.filter { case t => filter(tuples.head, t) }
      tuples.find { case t => !rules.exists { rule => ruleMatcher(t, rule) } }.map(_ => false).getOrElse(true)
    }
  }

  protected def checkRow(tuples: List[Rule], seenTuples: List[Rule] = List.empty): Boolean = {
    tuples match {
      case Nil => true
      case  head :: tail =>
        checkRules(head :: seenTuples, ruleEqualsRight, ruleEqualsLeft) &&
          checkRules(head :: tail, ruleEqualsLeft, ruleEqualsRight) &&
          checkRow(tail, head :: seenTuples )
    }
  }
}

trait DecemberFivePartOne extends DecemberFive with App{

  override def solve(): Int = {
    instuctionsTuples.filter{ case (i, tuples) => checkRow(tuples) }
     .flatMap{ case (instruction, _) => middle(instruction) }
     .foldLeft(0) { case(total, middle) => total + middle }
  }
}

object DecemberFivePartOneTest extends DecemberFivePartOne {
  override def test = true
}

object DecemberFivePartOneSolution extends DecemberFivePartOne {
  override def test = false
}
trait DecemberFivePartTwo extends DecemberFive with App {

  private def findCorrectPermutation(instructions:  List[Int]): Option[List[Int]] = {
    def permute(permutations: List[List[Int]]): Option[List[Int]] = {
      permutations match {
        case Nil => None
        case head :: tail =>
          val tuples = toTuples(head)
          if (checkRow(tuples)) Some(head)
          else permute(tail)
      }
    }
    permute(instructions.permutations.toList)
  }

  override def solve(): Int = {
    instuctionsTuples.filter{ case(_, tuples) => !checkRow(tuples) }
      .flatMap { case (instruction, _) =>
        findCorrectPermutation(instruction)
      }
      .flatMap{ permutation => middle(permutation) }
      .foldLeft(0) { case (a, b) => a + b }
  }
}


object DecemberFivePartTwoTest extends DecemberFivePartTwo {
  override def test = true
  println(solve())
}

object DecemberFivePartTwoSolution extends DecemberFivePartTwo {
  override def test = false
  println(solve())
}


//  private val list = List(75, 97, 47, 61, 53)
//  for {
//    tuples <- toTuples(list.headOption, list.tail)
//    ints <- findCorrectPermutation(tuples)
//  } yield { println(ints)}
