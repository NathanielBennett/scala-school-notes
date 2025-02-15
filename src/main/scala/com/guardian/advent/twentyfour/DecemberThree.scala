package com.guardian.advent.twentyfour

import com.guardian.advent.parsers.StringParser


trait DecemberThree[S] extends December[S, List[String], S] with StringParser  {

  override def day: Int = 3

  val input: List[String] = rawInput

  protected val dont = "don't()"
  protected val doo = "do()"

  private val m = """mul\((\d+),(\d+)\)""".r

  protected def multiply(list: List[String]): List[Int] = list.flatMap{ s => multsForString(s) }
  protected def multsForString(s: String): List[Int] = {
    m.findAllIn(s).toList
      .map { l =>
        val ps = m.findAllIn(l)
        ps.group(1).toInt * ps.group(2).toInt
      }
  }
}

trait DecemberThreePartOne extends DecemberThree[Int] {

  override def solver: Solver[Int, Int] = listTotalSolver(0, test)
  override def rawSolution: List[Int] = input.flatMap{ s => multsForString(s) }
}

trait DecemberThreePartTwo extends DecemberThree[Long] {

  override def solver: Solver[Long, Long] = listTotalSolver(0L, test)

    override def rawSolution: List[Long] = {
      val rawInstructions = input.foldLeft(new StringBuilder()) { case (stringBuilder, string) => stringBuilder.append(string) }.toString()
      val delim = startStop(rawInstructions)
      val cleanInstructions = findInstructions(rawInstructions, delim)
        .flatMap { case (instructions, shouldProcess) => if (shouldProcess) Some(instructions) else None }
        .foldLeft(new StringBuilder()) { case (stringBuilder: StringBuilder, string: String) => stringBuilder.append(string) }
        .toString
      multsForString(cleanInstructions).map(_.toLong)
    }

    def makeNextDelim(delim: String): String = if (delim == dont) doo else dont

    def end(rawInstructions: String) = rawInstructions.indexOf(doo) < 0 && rawInstructions.indexOf(dont) < 0

    def startStop(rawInstructions: String): String =
      rawInstructions.indexOf(dont) < rawInstructions.indexOf(doo) match {
        case true => dont
        case false => doo
      }

    def process(delim: String) = delim == dont

    def makeNextInstructions(rawInstructions: String, delim: String): String = {
      if (end(rawInstructions)) "" else rawInstructions.substring(rawInstructions.indexOf(delim) + delim.length)
    }

    def findInstructions
    (rawInstructions: String, delim: String, cleanInstructions: List[(String, Boolean)] = List.empty): List[(String, Boolean)] = {

      def makeListedInstructions: String =
        if (end(rawInstructions)) rawInstructions else rawInstructions.substring(0, rawInstructions.indexOf(delim))

      if (rawInstructions.isEmpty) cleanInstructions.reverse
      else {
        val nextDelim = makeNextDelim(delim)
        val nextRawInstructions = makeNextInstructions(rawInstructions, delim)
        findInstructions(
          nextRawInstructions,
          nextDelim,
          (makeListedInstructions, process(delim)) :: cleanInstructions
        )
      }
    }
}

object DecemberThreePartOneTest extends DecemberThreePartOne with PuzzleTest
object DecemberThreePartOneSolution extends DecemberThreePartOne with PuzzleSolution
object DecemberThreePartTwoTest extends DecemberThreePartTwo with PuzzleTest
object DecemberThreePartTwoSolution extends DecemberThreePartTwo with PuzzleSolution
