package com.guardian.advent

import scala.io.Source

trait AdventOfCode {

  val rootPath = s"${System.getProperty("user.home")}/advent2023/2024"

  def day: Int

  private def parseLinesFromResource[T](test: Boolean = false)(parser: String => Option[T] ): List[T] = {
    println(test)
    val fileName = makeFilename(test)
    Source.fromResource(s"advent/2024/$fileName").getLines().toList
      .flatMap { line => parser(line)}
  }

  private def makeFilename(test: Boolean): String = test match {
    case true => s"day_${day}_test.txt"
    case false => s"day_$day.txt"
  }

  def lineParser[T](test: Boolean = false)(parser: String => Option[T]): List[T] = {
    val fileName = test match {
      case true => s"day_${day}_test.txt"
      case false => s"day_$day.txt"
    }

    Source.fromFile(s"$rootPath/$fileName").getLines().toList
      .flatMap { line => parser(line) }
  }

  def gridParser[T](test: Boolean)(entryParser: (Int, Int, Char)  => Option[GridEntry[T]], gridMaker: Set[GridEntry[T]] => Grid[T] ) : Grid[T] = {
   val s = parseLinesFromResource[String](test)(s => Some(s))
     .zipWithIndex
     .flatMap{
        case(rawEntries, yIndex) =>
          rawEntries.toCharArray.toList.zipWithIndex.flatMap {
            case (char, xIndex) => entryParser(xIndex, yIndex, char)
          }
     }
     gridMaker(s.toSet)
  }
}

