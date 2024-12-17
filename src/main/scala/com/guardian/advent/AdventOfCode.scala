package com.guardian.advent

import scala.io.Source

trait AdventOfCode {

    val rootPath = s"${System.getProperty("user.home")}/advent2023/2024"
    def day: Int

    def lineParser[T](test: Boolean = false)(parser: String => Option[T]) : List[T] = {
      val fileName = test match {
        case true => s"day_${day}_test.txt"
        case false => s"day_$day.txt"
      }

      Source.fromFile(s"$rootPath/$fileName").getLines().toList
        .flatMap{line => parser(line)}
    }

    def gridParser[S, T <: GridEntry[S]](test = false)(parseRow: String => List[T], makeGrid: List[T] => Grid[S, T]): Grid[S, T] = {
      val entries = lineParser[String](test){ s => Some(s) }
        .flatMap{ s => parseRow(s) }

      makeGrid(entries)
    }
}