package com.guardian.advent.twentyfour

import com.guardian.advent.AdventOfCode

trait DecemberThree[T] extends AdventOfCode[T] with App {
  override def day: Int = 3
  val m = """mul\((\d+),(\d+)\)""".r

  val dont = "don't()"
  val doo = "do()"
  val input = lineParser[String]() { s => Some(s) }
    println(input.size)

  def multsForString(s: String): List[Int] = {
    m.findAllIn(s).toList
      .map { l =>
        val ps = m.findAllIn(l)
        ps.group(1).toInt * ps.group(2).toInt
      }
  }
}

object DecemberThreeOneTest extends DecemberThree[Int] {

  override def test: Boolean = true

  def solve(): Int = {
    input
      .flatMap { s => multsForString(s) }
      .foldLeft(0){ case(a, b) => a + b }
  }
}

object DecemberThreeTwoTest extends DecemberThree[Long] {

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
      .foldLeft ( new StringBuilder() ) { case (stringBuilder: StringBuilder, string: String) => stringBuilder.append(string) }
      .toString

    multsForString(cleanInstructions)
      .foldLeft(0L) { case (total, i) => total + i }
  }
}