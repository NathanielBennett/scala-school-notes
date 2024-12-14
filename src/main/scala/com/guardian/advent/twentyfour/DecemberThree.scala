package com.guardian.advent.twentyfour

trait DecemberThree extends AdventOfCode with App {
  override def day: Int = 3
  val m = """mul\((\d+),(\d+)\)""".r

  val input = lineParser[String]() { s => Some(s)}
    println(input.size)

  def multsForString(s: String): List[Int] = {
    m.findAllIn(s).toList
      .map { l =>
        val ps = m.findAllIn(l)
        ps.group(1).toInt * ps.group(2).toInt
      }
  }
}

object DecemberThreeOne extends DecemberThree {

  val total = input
    .flatMap { s => multsForString(s) }
    .foldLeft(0){ case(a, b) => a + b}

  println(total)
}