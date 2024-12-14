import scala.io.Source
import scala.util.Try

val figureMatcher = """^(\d+)\s+(\d+)$""".r
val l = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/2024/day_2_test.txt").getLines().toList
  .map {
    s => s.split(" ").toList
      .map(_.toInt)
  }
