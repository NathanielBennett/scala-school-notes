import scala.io.Source
import scala.util.Try

val cardR = """^(\w+:+\s+)(.*)$""".r

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day6.txt").getLines().toList

def parseRaces(line: String): Option[List[Int]] = Try {
  val cardR(_, times) = line
  """\s+""".r.split(times).toList.map(_.toInt)
}.toOption

val races = for {
   time <- parseRaces(lines(0))
   distance <- parseRaces(lines(1))
} yield {
  time.zip(distance).map { case (time, distance) =>
    (1 until time).toList.map { hold =>
      hold * (time - hold)
    }
      .filter(i => i > distance)
  }
  .map{l => l.size}
    .foldLeft(1){case(a, b) => a * b }

}

def parseOneRace(line: String): Option[Long] = Try {
  val cardR(_, times) = line
  """\s+""".r.split(times).toList.foldLeft(new StringBuilder()) {case(sb, s) => sb.append(s)}.toString.toLong
}.toOption

parseOneRace(lines(0))
parseOneRace(lines(1))
val races2 = for {
  time <- parseOneRace(lines(0))
  distance <- parseOneRace(lines(1))
} yield {
  (1L until time).toList.map {
    hold => hold * (time - hold)
  }
  .filter( i => i > distance)
  .size
}


/*
val times = Try {
}.toOption
*/

