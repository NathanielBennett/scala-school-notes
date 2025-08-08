package com.guardian.advent.twentyfour.util

import com.guardian.advent.twentyfour.{CharEntry, Space}
import com.guardian.advent.{Cardinal, InputFileReader}
import com.guardian.advent.twentyfour.DecemberSixRefactorPartOneTest.PathEntry

import scala.util.Try

trait SubFileReader extends InputFileReader {
  override def test: Boolean = false
  override def day: Int = -1
  def lines = getLines()

  val entryR = """^\(\d+\)\s\(?Pos\((\d+),(\d+)\),(\w+)\)$""".r

  def parse(): List[PathEntry] = {
      lines.flatMap { line =>
      val maybeRaw =  Try {
        val entryR(x, y, cardinal) = line
        (x.toInt, y.toInt, cardinal)
      }.toOption

      for {
        (x, y, cardinalString) <- maybeRaw
        cardinal <- Cardinal(cardinalString)
      } yield (Space(x, y, '.'), cardinal)
    }
  }
}

object DecSixDiffer {

    val cheatReader = new SubFileReader  {
      override lazy val resourceName: String = "debug/6/path/cheat.txt"
    }
    val localReader = new SubFileReader {
      override lazy val resourceName: String = "debug/6/path/mine.txt"
    }

    val cheatEntries = cheatReader.parse
    val localEntries = localReader.parse

    def missing = cheatEntries.diff(localEntries)
}
