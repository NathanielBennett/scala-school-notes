package com.guardian.advent

import scala.io.Source

trait AdventOfCode {

  val rootPath = s"${System.getProperty("user.home")}/advent2023/2024"

  def day: Int

  private def parseLinesFromReaouexw[T](test: Boolean = true)(parser: String => Option[T] ): List[T] = {
    val fileName = makeFilename(test)
    Source.fromResource(s"2024/$fileName").getLines().toList
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

  def gridParser[T](test: Boolean = false)(entryParser: (Char, Int, Int)  => Option[T], gridMaker: Set[T] => Grid[T] ) : Grid[T] = {
   val s = parseLinesFromReaouexw[String](test)(s => Some(s))
     .zipWithIndex
     .flatMap{
        case(rawEntries, yIndex) =>
          rawEntries.toCharArray.toList.zipWithIndex.flatMap {
            case (char, xIndex) => entryParser(char, xIndex, yIndex)
          }
     }
     gridMaker(s.toSet)
  }
}
