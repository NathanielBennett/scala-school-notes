import scala.io.Source
import scala.util.Try

val areas = """^(\d{1,2})-(\d{1,2}),(\d{1,2})-(\d{1,2})$""".r

def makeRange(start: String, end: String): Option[Range] = {
  for{
    startInt <- Try(start.toInt).toOption
    endInt <- Try(end.toInt).toOption
  } yield startInt to endInt
}

def parseRanges(s: String): Option[(Range, Range)] = s match {
  case areas( startOne, endOne, startTwo, endTwo) =>
    for {
      firstRange <- makeRange(startOne,endOne)
      secondRange <- makeRange(startTwo, endTwo)
    } yield (firstRange, secondRange)
}


def plotsContainsAllOf(plotsOne: Range, plotsTwo: Range) : Boolean =
  plotsOne.forall(plot => plotsTwo.contains(plot)) || plotsTwo.forall(plot => plotsOne.contains(plot))



val duplicatedPlots = Source.fromFile(s"${System.getProperty("user.home")}/plots.txt").getLines()
  .toList
  .flatMap(line => parseRanges(line))
  .map{ case(plotsOne, plotsTwo) => plotsContainsAllOf(plotsOne, plotsTwo)}
  .filter(plot => plot)
  .size

