package com.guardian.advent

import scala.io.Source

trait AdventOfCode {

  val rootPath = s"${System.getProperty("user.home")}/advent2023/2024"

  def day: Int

  private def parseLines[T](fileName: String)(parser: String => Option[T]) : List[T] =
    Source.fromFile(fileName).getLines().toList
      .flatMap { line => parser(line)}

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

  def gridParser[R, S, T <: GridEntry[S]](test: Boolean = false)(
    lineParser: String => Option[R], makeGrid: Set[S] => Grid[T]): Grid[T] = {
       val fileName = makeFilename(test)
       val gridEntries = parseLines[S](fileName)(lineParser).toSet
       makeGrid(gridEntries)
  }
}
