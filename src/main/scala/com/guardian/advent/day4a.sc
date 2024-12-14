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

val cards = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day4.txt").getLines().toList
  .flatMap { line => inputLineToTuple(line) }
val totals  = cards.flatMap { case (cardNumbers, winningNumbers) =>
    for {
      card <- makeNumberSet(cardNumbers, cardR)
      winners <- makeNumberSet(winningNumbers, winningR)
    } yield card.intersect(winners)
  }


val totalCards = cards.zip(totals).map { case (a, b) => (a, b.length) }.zipWithIndex
  .map { case((card, nWinningNumbers), index) => (card, nWinningNumbers, index + 1) }

def duplicateCards(
    scratchCardsWithTotalOfWinningNumbers: List[((String, String), Int, Int)],
    copiedCardsToProcesss: List[((String, String), Int, Int)] = List.empty,
    processedCards: List[(String, String)] = List.empty ) : List[(String, String)] =
  scratchCardsWithTotalOfWinningNumbers match {
    case Nil => processedCards
    case head :: tail =>
         copiedCardsToProcesss match {
           case Nil => val (scratcCard, nWinningNumbers, index) = head
             val copies = tail.slice(index, index + nWinningNumbers)
             duplicateCards(tail, copies, scratcCard :: processedCards  )
           case dupeHead :: dupeTail =>
              val (scratcCard, nWinningNumbers, index) = dupeHead
              val moreCopies = tail.slice(index, nWinningNumbers )
              duplicateCards(scratchCardsWithTotalOfWinningNumbers, dupeTail ::: moreCopies, scratcCard :: processedCards)
         }
  }

val allCards = duplicateCards(totalCards).size

val total = totals
  .map { case winners => winners match {
    case Nil => 0
    case List(x) => 1
    case _ :: tail => tail.foldLeft(1){case(total, _) => total * 2}
  }}

val totalC = total.foldLeft(0){case(total, winnings) => total + winnings}

