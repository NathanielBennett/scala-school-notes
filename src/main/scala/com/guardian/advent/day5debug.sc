import com.guardian.advent.TestObject._

import scala.io.Source
import scala.util.Try


def parseRangeSet(list: List[String]): Try[List[(Long, Long, Long)]] = Try {
  val sortedTuples = list.map{s => numberStringToLongTuple(s)}
    .sortBy {case (start, _, _) => start}

  sortedTuples.foldLeft(List[(Long, Long, Long)]()) { case (rangesSoFar, newRange) =>
    val (sourceRangeStart, destRangeStart, rangeLength) = newRange
    rangesSoFar match {
      case Nil => List((sourceRangeStart, sourceRangeStart + rangeLength-1, destRangeStart))
      case head :: tail =>
        val (curSourceRangeStart, curSourceRangeEnd, curDestRangeStart) = head
        if (curSourceRangeEnd == sourceRangeStart ) {
          (curSourceRangeStart, curSourceRangeEnd + rangeLength, curDestRangeStart) :: tail
        } else {
          (sourceRangeStart, sourceRangeStart + rangeLength, destRangeStart) :: rangesSoFar
        }
    }
  }.reverse
}

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day5test.txt").getLines().toList
val seedMaps = parseInputToListOfLists(lines.tail.tail)

val h = seedMaps.head
parseRangeSet(h)