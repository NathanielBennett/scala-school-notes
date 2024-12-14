import com.guardian.advent.TestObject.totalCards

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex


val cardR = """^Card\s+\d+:(.*)$""".r
val winningR = """^(.*)$""".r

def inputLineToTuple(line: String): Option[(String, String)] = Try {
  val split = line.split('|')
  (split(0).trim, split(1).trim)
}.toOption

def makeNumberSet(numberSet: String, regex: Regex): Option[List[String]] = Try{
  val regex( numbers ) = numberSet
  """\s+""".r.split(numbers).toList
}.toOption

val cards = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day4r.txt").getLines().toList
  .flatMap { line => inputLineToTuple(line) }
val totals  = cards.flatMap { case (cardNumbers, winningNumbers) =>
  for {
    card <- makeNumberSet(cardNumbers, cardR)
    winners <- makeNumberSet(winningNumbers, winningR)
  } yield card.intersect(winners)
}

val totalCards = cards.zip(totals).map { case (a, b) => (a, b.length) }.zipWithIndex
  .map { case((card, nWinningNumbers), index) => (card, nWinningNumbers, index + 1) }

val totalsMap = totalCards.map{
  case (_, winningNumbers, index) => (s"Card $index", winningNumbers)
}.toMap

val totalCardNumber = totalCards
  .foldLeft(Map[String, Int]()) { case (totalsMap, (_, winningNumbers, index)) =>
    val currentCardKey = s"Card $index"
    val updatedTotalForCurrentCard = totalsMap.getOrElse(currentCardKey, 0) + 1
    val extraCards =(1 to updatedTotalForCurrentCard).toList.flatMap { _ =>
      totalCards.slice(index, index + winningNumbers)
        .map { case (_, _, bonusIndex) => s"Card $bonusIndex" }
    }

    extraCards.foldLeft(totalsMap) {
      case (totals, cardName) =>
        val newTotal = totals.getOrElse(cardName,0) + 1
        totals + (cardName -> newTotal)
    } + (currentCardKey -> updatedTotalForCurrentCard)
  }.foldLeft(0){ case(total, (_, cardTotal)) => total + cardTotal }

