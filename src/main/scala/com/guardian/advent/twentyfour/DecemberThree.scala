package com.guardian.advent.twentyfour

import com.guardian.advent.InputFileReader
import com.guardian.advent.parsers.StringParser

trait DecemberThree[T] extends StringParser with InputFileReader {

  override def day: Int = 3

  val lines = getLines()
  val input = parseLinesFromResource(lines)

  protected val dont = "don't()"
  protected val doo = "do()"

  private val m = """mul\((\d+),(\d+)\)""".r

  def multsForString(s: String): List[Int] = {
    m.findAllIn(s).toList
      .map { l =>
        val ps = m.findAllIn(l)
        ps.group(1).toInt * ps.group(2).toInt
      }
  }
}

trait DecemberThreePartOne extends DecemberThree[Int] {
  def solve(): Int = {
    input
      .flatMap { s => multsForString(s) }
      .foldLeft(0){ case(a, b) => a + b }
  }
}

object DecemberThreePartOneTest extends DecemberThreePartOne {
  override def test: Boolean = true
}

object DecemberThreePartOneSolution extends DecemberThreePartOne {
  override def test: Boolean = false
}

trait DecemberThreePartTwo extends DecemberThree[Long] {

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

   override def solve(): Long = {
      val rawInstructions = input.foldLeft(new StringBuilder()) { case (stringBuilder, string) => stringBuilder.append(string) }.toString()
      val delim = startStop(rawInstructions)
      val cleanInstructions = findInstructions(rawInstructions, delim)
        .flatMap { case (instructions, shouldProcess) => if (shouldProcess) Some(instructions) else None }
        .foldLeft(new StringBuilder()) { case (stringBuilder: StringBuilder, string: String) => stringBuilder.append(string) }
        .toString

      multsForString(cleanInstructions)
        .foldLeft(0L) { case (total, i) => total + i }
    }
}

object DecemberThreePartTwoTest extends DecemberThreePartTwo {
  override def test: Boolean = true
}

object DecemberThreePartTwoSolution extends DecemberThreePartTwo {
  override def test: Boolean = false
}
