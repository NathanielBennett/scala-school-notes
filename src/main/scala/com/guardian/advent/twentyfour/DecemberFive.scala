package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.{IntegerListParser, IntegerTupleParser}
import com.guardian.advent.{AdventOfCodeGridParser, AdventOfCodeInstructionsParser, AdventOfCodeParser}

import scala.util.Try

trait DecemberFiveParser extends AdventOfCodeInstructionsParser[ (Int, Int), List[(Int, Int)], List[Int], List[List[Int]] ] {

  override def inputParser = new IntegerTupleParser {
    override def lineParser(line: String): Option[(Int, Int)] = {
      val l = line.toStringList('/')
      listToTuple(l)
    }
  }

  override def instructionParser = new IntegerListParser {
    override def lineParser(line: String): Option[List[Int]] = {
      val l = line.toStringList(',')
      listToIntList(l)
    }
  }
}


trait DecemberFive extends DecemberFiveParser {

  override def day: Int = 5

  type Rule = (Int, Int)
  type RuleMatcher = (Rule, Rule) => Boolean

  val (input, instructions) = parser()
  val instuctionsTuples = instructions.map { instruction =>
    val tuples = toTuples(instruction)
    (instruction, tuples)
  }

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
