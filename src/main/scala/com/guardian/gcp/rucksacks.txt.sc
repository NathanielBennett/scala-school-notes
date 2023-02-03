
import scala.io.Source

case class Rucksack(rawString: String) {
  def score() = {
    val len = rawString.length
    val compartmentOne = rawString.substring(0, len / 2).toCharArray.toList
    val compartmentTwq = rawString.substring(len / 2).toCharArray.toList
    val common = compartmentOne.intersect(compartmentTwq)
    common.headOption.flatMap(c => Rucksack.priorities.get(c))
  }
}

object Rucksack {
  val priorities = ('a' to 'z').appendedAll('A' to 'Z')
    .toList
    .zipWithIndex
    .map{ case (ch, index) => (ch, index + 1) }
    .toMap
}

val totalRucksackPriority = Source.fromFile(s"${System.getProperty("user.home")}/rucksacks.txt").getLines()
  .toList
  .flatMap(l => Rucksack(l).score())
  .foldLeft(0){ case (total, score) => total + score}


