
import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.Try

implicit class RichBoolean(val b: Boolean) {
  def option[A](a: => A): Option[A] = if (b) Some(a) else None
}

def makeNumberList(numbers: String): Option[LazyList[Long]] = Try {

  def tupleLoop(l: List[Long], maybeStart: Option[Long] = None, acc: LazyList[NumericRange.Exclusive[Long]] = LazyList.empty): LazyList[NumericRange.Exclusive[Long]] = {
    l match {
      case Nil => acc
      case head :: tail =>
        maybeStart match {
          case Some(rangeStart) =>
            println(s"Making range $rangeStart to ${rangeStart + head}")
            tupleLoop(tail, None, (rangeStart until rangeStart + head) #:: acc)
          case _ =>
            println(s"Tagging new rangs starting $head")
            tupleLoop(tail, Some(head), acc)
        }
    }
  }

  val longList = """\s+""".r.split(numbers).toList.map(_.toLong)
  tupleLoop(longList).flatMap { range =>
    println(s"Adding range ${range.start} to ${range.end}")
    LazyList.range(range.start, range.end) }
}.toOption

val seedsR = """seeds:\s+(.+)""".r

def parseSeeds(line: String): Option[LazyList[Long]] = for {
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


case class SeedMapSegment(sourceRange: NumericRange.Exclusive[Long], destRangeStart: Long) {
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

def numberStringToLongTuple(numberString: String): (Long, Long, Long) = {
  val mapDescriptorR(destinationRangeStart, sourceRange, rangeLength) = numberString
  (sourceRange.toLong, destinationRangeStart.toLong, rangeLength.toLong)
}

//bkl
def listOfStingToSeedMap(list: List[String]): Option[SeedMap] = Try {
  println("Parsing seedMap")
  val segments = list.map { numberString =>
    println(s"Parsing seedmap entry: $numberString")
    val (sourceRangeStart, destRangeStart, rangeLength) = numberStringToLongTuple(numberString)
    SeedMapSegment(sourceRangeStart until sourceRangeStart + rangeLength, destRangeStart)
  }
  SeedMap(segments)
}.toOption

def listOfTupleToSeedMap(tuples: List[(Long, Long, Long)]): SeedMap = {
  SeedMap( tuples.map { case(start, end, destStart) => SeedMapSegment(start until end, destStart) } )


  def listOfStringToRangeSet(list: List[String]): Option[List[(Long, Long, Long)]] = Try {
    val sortedTuples = list.map{s => numberStringToLongTuple(s)}
      .sortBy {case (start, _, _) => start}

    sortedTuples.foldLeft(List[(Long, Long, Long)]()) { case (rangesSoFar, newRange) =>
      val (sourceRangeStart, destRangeStart, rangeLength) = newRange
      rangesSoFar match {
        case Nil => List((sourceRangeStart, sourceRangeStart + rangeLength, destRangeStart))
        case head :: tail =>
          val (curSourceRangeStart, curSourceRangeEnd, curDestRangeStart) = head
          if (curSourceRangeEnd == sourceRangeStart) {
            (curSourceRangeStart, curSourceRangeEnd + rangeLength, curDestRangeStart) :: tail
          } else {
            (sourceRangeStart, sourceRangeStart + rangeLength, destRangeStart) :: rangesSoFar
          }
      }
    }.reverse
  }.toOption

  def findValueForSeed(seed: Long, seedMaps: List[SeedMap], mapNo: Int = 1): Long =
    seedMaps match {
      case Nil => seed
      case head :: tail =>
        println(s"Looking for seed $seed in map $mapNo")
        val nextSeed = head.findNextTarget(seed)
        findValueForSeed(nextSeed, tail, mapNo + 1)
    }

  val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day5.txt").getLines().toList
  val l = parseSeeds(lines.head).map { seeds =>

    val seedMaps = parseInputToListOfLists(lines.tail.tail)
      .flatMap(l => listOfStringToRangeSet(l))
      .map { tupleList => listOfTupleToSeedMap(tupleList) }
    seeds.map { seed => findValueForSeed(seed, seedMaps) }.min
  }
  println("***")