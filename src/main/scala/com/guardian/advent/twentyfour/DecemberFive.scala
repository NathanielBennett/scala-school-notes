 package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCodeInstructionsParser
import com.guardian.advent.parsers.{IntegerListParser, IntegerTupleParser}

import java.util.Locale.{LanguageRange, setDefault}
import javax.print.attribute.standard.MediaSize.Other
import scala.collection.AbstractSeq
import scala.collection.View.{Filter, empty}
import scala.collection.immutable.LinearSeq

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
  override def solver: Solver[Int, Int] = listTotalSolver(0, test):

  def defaultList: LinearSeq[Rule] = List.empty

  val (input, instructions) = rawInput
  val instuctionsTuples = instructions.map { instruction =>
    val tuples = toTuples(instruction)
    (instruction, tuples)
  }

  val leftInput = input.listMapLeft
  val rightInput = input.listMapRight

  protected def toTuples[A <: LinearSeq[Int], B <: LinearSeq[Rule] ](others: A, acc: B = defaultList : B = {
    if (others.isEmpty) acc
    else others.tailEmpty match {
      case true => toTuples(List.empty, acc)
      case false =>
        toTuples(others.tail, acc.appended(others.head, others.tailHead).asInstanceOf[B])
    }
  }
  protected def middle[A <: LinearSeq[Int]](list: A, last: Option[Int] = None, cnt: Int = 0 ): Option[Int] = {
    list.headOption.flatMap {
      head =>
        if (cnt > list.size) last
        else middle(list.tail, Some(head), cnt + 1)
    }
  }

  private def checkRulesMap(lefts: List[Int], head: Int,  ruleMap: Map[Int, List[Int]]) : Boolean = {

    lefts.isEmpty | ruleMap.get(head).map { case ls =>
       lefts.forall{ left => (head :: ls).contains(left) }
    }.getOrElse(true)
  }

  protected def checkRow[A <: LinearSeq[Rule](tuples: A): Boolean = {
    if (tuples.isEmpty) true
    else {
        checkRulesMap(tuples.rightList, tuples.head.left, leftInput) &&
          checkRulesMap(tuples.leftList, tuples.last.right, rightInput) &&
          checkRow(tuples.tail)
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
          val tuples = toTuples(permutation, List.empty)
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
