 package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeInstructionsParser
import com.guardian.advent.parsers.{IntegerListParser, IntegerTupleParser}

trait DecemberFiveParser extends AdventOfCodeInstructionsParser[ (Int, Int), List[(Int, Int)], List[Int], List[List[Int]] ] {
1
  type Rule = (Int, Int)

  class IntegerTupleParserImp(override val day: Int, override val test: Boolean) extends IntegerTupleParser {
    override def lineParser(line: String): Option[Rule] = {
      val l = line.toStringList('|')
      listToTuple(l)
    }
  }

  class IntegerListParserImp(override val day: Int, override val test: Boolean) extends IntegerListParser {
    override def lineParser(line: String): Option[List[Int]] = {
      val l = line.toStringList(',')
      listToIntList(l)
    }
  }

  override def inputParser = new IntegerTupleParserImp(day, test)
  override def instructionParser = new IntegerListParserImp(day, test)
}


trait DecemberFive extends December[Int, (List[(Int, Int)], List[List[Int]]), Int] with DecemberFiveParser {

  type RuleMatcher = (Rule, Rule) => Boolean

  override def day = 5
  override def solver: Solver[Int, Int] = listTotalSolver(0, test)

  val (input, instructions) = rawInput
  val instuctionsTuples = instructions.map { instruction =>
    val tuples = toTuples(instruction)
    (instruction, tuples)
  }

  protected def toTuples(others: List[Int], lastHead: Option[Int] = None, acc: List[Rule] = List.empty) : List[Rule] = {
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

trait DecemberFivePartOne extends DecemberFive  {

  override def rawSolution: List[Int] =
    instuctionsTuples
      .filter { case(_, rule) => checkRow(rule) }
      .flatMap{ case(instructions, _) => middle(instructions)}
}

object DecemberFivePartOneTest extends DecemberFivePartOne with PuzzleTest
object DecemberFivePartOneSolution extends DecemberFivePartOne with PuzzleSolution

trait DecemberFivePartTwo extends DecemberFive {

  private def findCorrectPermutation(instructions: List[Int]): Option[List[Int]] = {
    def permute(iterator: Iterator[List[Int]]): Option[List[Int]] = {

      iterator.next match {
        case Nil => None
        case permutation =>
          val tuples = toTuples(permutation)
          if (checkRow(tuples)) Some(permutation)
          else permute(iterator)
      }
    }

    permute(instructions.permutations)
  }

  override def rawSolution: List[Int] = {
    instuctionsTuples.filter{ case(_, rule) => !checkRow(rule)}
      .flatMap { case (instructions, _) =>  findCorrectPermutation(instructions) }
      .flatMap { permutation => middle(permutation)}
  }
}

object DecemberFivePartTwoTest extends DecemberFivePartTwo with PuzzleTest

object DecemberFivePartTwoSolution extends DecemberFivePartTwo with PuzzleTest with App {
  println(solve)
}


//  private val list = List(75, 97, 47, 61, 53)
//  for {
//    tuples <- toTuples(list.headOption, list.tail)
//    ints <- findCorrectPermutation(tuples)
//  } yield { println(ints)}
