import scala.io.Source
import scala.util.Try

sealed trait Hand extends Ordered[Hand] {
  def cards: String
  def bid: Int
  def typeRank: Int
  def cardValues: List[Int] = cards.flatMap{ ch => Try{
    ch match {
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
      case _ => s"$ch".toInt
    }
  }.toOption}.toList

  override def compare(that: Hand): Int  = {
   def compareCardValues(theseCards: List[Int], thoseCards: List[Int]): Int =
    theseCards match {
      case Nil => 0
      case head :: tail =>
        val eq = head compareTo (thoseCards.head)
        if (eq == 0) compareCardValues(tail, thoseCards.tail) else eq
    }
    compareCardValues(cardValues, that.cardValues)
  }
}

object Hand {
  def apply(rawCards: String, bid: Int): Hand = {
    val groupedCards = rawCards.groupBy { c => c }
      .toList.map { case (_, groupedCards) => groupedCards }
      .sortBy(_.length)

    groupedCards match {
      case _ :: Nil => FiveOfAKind(rawCards, bid)
      case first :: _ :: Nil => first.length match {
        case 1 => FourOfAKind(rawCards, bid)
        case 2 => FullHouse(rawCards, bid)
      }
      case _ :: _ :: last :: Nil => last.length match {
        case 3 => ThreeOfAKind(rawCards, bid)
        case 2 => TwoPair(rawCards, bid)
      }
      case _ :: _ :: _ :: _ :: Nil => OnePair(rawCards, bid)
      case _ => HighCard(rawCards, bid)
    }
  }
}

case class FiveOfAKind(override val cards: String, override val bid: Int) extends Hand {
  override def typeRank: Int = 7
}

case class FourOfAKind(override val cards: String, override val bid: Int) extends Hand {
  override def typeRank: Int = 6
}

case class FullHouse(override val cards: String, override val bid: Int) extends Hand{
  override def typeRank: Int = 5
}

case class ThreeOfAKind(override val cards: String, override val bid: Int) extends Hand {
  override def typeRank: Int = 4
}

case class TwoPair(override val cards: String, override val bid: Int) extends Hand{
  override def typeRank: Int = 3
}

case class OnePair(override val cards: String, override val bid: Int) extends Hand{
  override def typeRank: Int = 2
}

case class HighCard(override val cards: String, override val bid: Int) extends Hand{
  override def typeRank: Int = 1
}


val handR = """^(\w+)\s+(\d+)$""".r

def getHandsAndBids(): List[(String, Int)] =
  Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day7.txt").getLines().toList
   .flatMap { line =>
     Try {
       val handR(cards, bid) = line
       (cards, bid.toInt)
     }.toOption
   }
Hand("JJJ28", 12).cardValues



val hands = getHandsAndBids().flatMap { case (hand, bid) => Try {
    Hand(hand, bid)
  }.toOption
}.groupBy( hand => hand.typeRank )
 .toList
 .sortBy{case( rank, _) => rank}
 .flatMap { case(_, hands) => hands.sorted }
 .zipWithIndex.toList
 .foldLeft(0){ case (total, (hand, index)) =>
   val winnings = (index + 1)  * hand.bid
   //println(s"Hand: ${hand}   Index: ${index + 1}. Winnings: ${winnings}" )
   total + winnings
 }

/*
val sorted = hands.map{case(_, hands) => hands.sorted.reverse }
val usorted = hands.map{case(_, hands) => hands }

val l = List( Hand("KTJJT", 2202), Hand("KK677", 222) ).reverse
val s = l.sorted

val l = List(2,5,3,1,12,2).sorted*/
