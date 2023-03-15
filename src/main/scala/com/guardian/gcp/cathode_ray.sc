import scala.io.Source
import scala.util.Try

case class Cycle(currentIndex: Int, currentXValue: Int, nextXValue: Int) {
  def shouldScore: Boolean = currentIndex % 20 == 0 && currentIndex % 40 != 0
  def score = currentIndex * nextXValue
}

sealed trait Signal {
  def listCycles(currentIndex: Int, currentXValue: Int, nextXValue: Int ): List[Cycle]
}

case object Noop extends Signal {

  override def listCycles(currentIndex: Int, currentXValue: Int, nextXValue: Int) =
    List(Cycle(currentIndex + 1, currentXValue, nextXValue))
}

case class AddX(signalStrength: Int, getNextXVal: (Int, Int) => Int) extends Signal {

  override def listCycles(currentIndex: Int, currentXValue: Int, nextXValue: Int) = {
    val nextX = getNextXVal(nextXValue, signalStrength)
    List(
        Cycle(currentIndex + 2, nextXValue, nextX),
        Cycle(currentIndex + 1, nextXValue, nextXValue),
    )
  }
}

object Signal {

  val addxMatch = """^addx\s(-?)(\d+)$""".r

  def apply(line: String): Option[Signal] = {
    val maybeSig: Option[Signal] = Try {
      line match {
        case "noop" => Noop
        case addxMatch("", strength) => AddX(strength.toInt, (nextXValue, signalStrength) => nextXValue + signalStrength)
        case addxMatch(_, strength) => AddX(strength.toInt, (nextXValue, signalStrength) => nextXValue - signalStrength)
      }
    }.toOption
    maybeSig
  }
}

val total = Source.fromFile(s"${System.getProperty("user.home")}/cathode_test.txt")
  .getLines
  .toList
  .flatMap{ line => Signal(line) }
  .foldLeft(List[Cycle]()) {
    case (cycles: List[Cycle], signal) =>
      val (currentIndex, currentXvalue, nextXValue) =
        cycles.headOption.map{ cycle => Cycle.unapply(cycle).get }.getOrElse(1, 1, 1)
      signal.listCycles(currentIndex, currentXvalue, nextXValue) ::: cycles
  }
  .filter(_.shouldScore)
  .foldLeft(0){ case (a, b) => a + b.score}




