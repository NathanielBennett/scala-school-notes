import scala.io.Source
import scala.util.Try

case class Card(label: Char, value: Int)

object Card {

  def charsToCardList(chars: String): Option[List[Card]] = Try {
    chars.map { ch =>
      val cardValue = ch match {
        case 'T' => 10
        case 'J' => 1
        case 'Q' => 12
        case 'K' => 13
        case 'A' => 14
        case _ => s"$ch".toInt
      }
      Card(ch, cardValue)
    }.toList
  }.toOption

}

sealed trait Hand extends Ordered[Hand] {
  lazy val cardTypeValues = "23456789TQKA"

  def bid: Int
  def typeRank: Int
  def cards: List[Card]

  override def toString: String = s"Card( ${cards.map(_.label).mkString} $bid )"

  override def compare(that: Hand): Int  = {

    def compareCardValues(theseCards: List[Card], thoseCards: List[Card]): Int = {
      theseCards match {
        case Nil => 0
        case head :: tail =>
          val eq = head.value compareTo (thoseCards.head.value)
          if (eq == 0) compareCardValues(tail, thoseCards.tail) else eq
      }
    }

    typeRank compareTo(that.typeRank) match {
      case 0 => compareCardValues(cards, that.cards)
      case x => x
    }
  }

  def maxHand: Hand = {
   cards.contains(Card('J', 1)) match {
     case false => this
     case true =>
       val allCardCombs = cards.foldLeft(List[List[Card]]()){
          case(cardLists, card) => cardLists match {
            case Nil if card == Card('J', 1) => cardTypeValues.map{ c => List(Card(c, 1)) }.toList
            case Nil => List(List(card))
            case _ if card == Card('J', 1) => cardLists.flatMap{
               theseCards => cardTypeValues.map{ ch => theseCards ::: List(Card(ch, 1))}
            }
            case _ => cardLists.map { cardList => cardList ::: List(card)}
          }
          }
       allCardCombs.map{ cardCombo => Hand(cardCombo, bid) }.sorted.reverse.head
   }
  }
}

object Hand {
  def apply(cards: List[Card], bid: Int): Hand = {
    val groupedCards = cards.groupBy { c => c.label }
      .toList.map { case (_, groupedCards) => groupedCards }
      .sortBy(_.length)

    groupedCards match {
      case _ :: Nil => FiveOfAKind(cards, bid)
      case first :: _ :: Nil => first.length match {
        case 1 => FourOfAKind(cards, bid)
        case 2 => FullHouse(cards, bid)
      }
      case _ :: _ :: last :: Nil => last.length match {
        case 3 => ThreeOfAKind(cards, bid)
        case 2 => TwoPair(cards, bid)
      }
      case _ :: _ :: _ :: _ :: Nil => OnePair(cards, bid)
      case _ => HighCard(cards, bid)
    }
  }
}

case class FiveOfAKind(override val cards: List[Card], override val bid: Int) extends Hand {
  override def typeRank: Int = 7
}

case class FourOfAKind(override val cards: List[Card], override val bid: Int) extends Hand {
  override def typeRank: Int = 6
}

case class FullHouse(override val cards: List[Card], override val bid: Int) extends Hand{
  override def typeRank: Int = 5
}

case class ThreeOfAKind(override val cards: List[Card], override val bid: Int) extends Hand {
  override def typeRank: Int = 4
}

case class TwoPair(override val cards: List[Card], override val bid: Int) extends Hand{
  override def typeRank: Int = 3
}

case class OnePair(override val cards: List[Card], override val bid: Int) extends Hand{
  override def typeRank: Int = 2
}

case class HighCard(override val cards: List[Card], override val bid: Int) extends Hand{
  override def typeRank: Int = 1
}

val handR = """^(\w+)\s+(\d+)$""".r

def getHand(line: String): Option[Hand] = for{
    (chars, bid) <- Try{
      val handR(cards, bid) = line
      (cards, bid.toInt)
    }.toOption
    cards <- Card.charsToCardList(chars)
  } yield Hand(cards, bid).maxHand

//245334887
//244848487

val hands = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day7.txt").getLines().toList
 .flatMap{ line => getHand(line) }
 .sorted
 .zipWithIndex
 .foldLeft(0) { case (total, (hand, index)) =>
    val warnings = (index + 1) * hand.bid
    total + warnings
 }


