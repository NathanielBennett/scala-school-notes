import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

implicit class RichBoolean(val b: Boolean) {
  def option[A](a: => A): Option[A] = if(b) Some(a) else None
}

def makeNumberList(numbers: String): Option[LazyList[Long]] = Try {

  def tupleLoop(l: List[Long], maybeStart: Option[Long] = None, acc: LazyList[NumericRange.Exclusive[Long]] = LazyList.empty ): LazyList[NumericRange.Exclusive[Long]] =
    l match {
      case Nil => acc
      case head :: tail =>
        maybeStart match {
          case Some(rangeStart) =>
            tupleLoop(tail, None, (rangeStart until rangeStart + head) #:: acc)
          case _ => tupleLoop(tail, Some(head), acc)
        }
    }

  val longList =  """\s+""".r.split(numbers).toList.map(_.toLong)
  tupleLoop(longList).flatMap { range => LazyList.range(range.start, range.end) }
}.toOption

val seedsR = """seeds:\s+(.+)""".r

def parseSeeds(line: String): Option[LazyList[Long]] =  for {
  seedNumbers <- Try {
    val seedsR(seedNumbers) = line
    seedNumbers
  }.toOption
  seeds <- makeNumberList(seedNumbers)
} yield seeds


def parseInputToListOfLists(lines: List[String], currentList: List[String] = List.empty, acc: List[List[String]] = List.empty): List[List[String]] =
  lines match {
    case Nil => acc.reverse
    case head :: tail =>
      head match {
        case "" => parseInputToListOfLists(tail, Nil, currentList.reverse :: acc)
        case s if s.head.isLetter => parseInputToListOfLists(tail, currentList, acc)
        case _ => parseInputToListOfLists(tail, head :: currentList, acc)
      }
  }

val mapDescriptorR = """^(\d+)\s(\d+)\s(\d+)$""".r

def listOfStringToTuple(list: List[String]): Option[List[(Long, Long, Long)]] = Try {
  list.map { numberString =>
    val mapDescriptorR(destinationRangeStart, sourceRange, start) = numberString
    (destinationRangeStart.toLong, sourceRange.toLong, start.toLong)
  }
}.toOption




7
case class SeedMapSegment(sourceRange: NumericRange.Exclusive[Long], destRangeStart: Long, rangeSize: Long) {
  def contains(target: Long): Option[Long] = {
    val offset = target - sourceRange.start
    sourceRange.contains(target).option(destRangeStart + offset)
  }
}

case class SeedMap(segments: List[SeedMapSegment]) {
 def findNextTarget(target: Long): Long = {
   def loop(segmentsRemaining: List[SeedMapSegment], target: Long): Long = segmentsRemaining match {
     case Nil => target
     case head :: tail => head.contains(target) match {
       case Some(nextTarget) => nextTarget
       case _ => loop(tail, target)
     }
   }
   loop(segments, target)
 }
}

def numberStringToLongTuple(numberString: String) : (Long, Long, Long) = {
  val mapDescriptorR(destinationRangeStart, sourceRange, start) = numberString
  (sourceRange.toLong, destinationRangeStart.toLong, start.toLong)
}

def listOfStingToSeedMap(list: List[String]): Option[SeedMap] = Try {
     val segments = list.map { numberString =>
       val (sourceRangeStart, destRangeStart, rangeSize) = numberStringToLongTuple(numberString)
       SeedMapSegment(sourceRangeStart until sourceRangeStart + rangeSize, destRangeStart, rangeSize )
     }
     SeedMap(segments)
  }.toOption

def findValueForSeed(seed: Long, seedMaps: List[SeedMap]): Long =
  seedMaps match {
    case Nil => seed
    case head :: tail =>
      val nextSeed = head.findNextTarget(seed)
      findValueForSeed(nextSeed, tail)
  }


val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day5test.txt").getLines().toList

val l = parseSeeds(lines.head).map { seeds =>
  val seedMaps = parseInputToListOfLists(lines.tail.tail)
    .flatMap(l => listOfStingToSeedMap(l))
  seeds.map { seed => findValueForSeed(seed, seedMaps) }.min
}


