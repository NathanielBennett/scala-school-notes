import scala.io.Source

sealed trait Turn {
  def rockScore: Int
  def scissorsScore: Int
  def paperScore: Int
  def result (opponentTurn: Turn) : Int = opponentTurn match {
    case Rock => rockScore
    case Paper => paperScore
    case Scissors => scissorsScore
  }
}

object Turn {

  def apply(s: String): Option[Turn] = s match {
    case "Rock" => Some(Rock)
    case "Paper" => Some(Paper)
    case "Scissors" => Some(Scissors)
    case _ => None
  }
}

case object Rock extends Turn {
  override def rockScore = 4
  override def scissorsScore = 9
  override def paperScore = 1
}

case object Paper extends Turn {
  override def rockScore = 8
  override def scissorsScore = 3
  override def paperScore = 3
}

case object Scissors extends Turn {
  override def rockScore = 3
  override def scissorsScore = 6
  override def paperScore = 9
}


val code: Map[String, String] = Map(
    "A" -> "Rock",
    "X" -> "Rock",
    "B" -> "Paper",
    "Y" -> "Paper",
    "C" -> "Scissors",
    "Z" -> "Scissors",
  )


val roundData = Source.fromFile(s"${System.getProperty("user.home")}/rock_paper_scissors.txt").getLines()
  .toList
  .flatMap {
    rawTurn =>
      val rawTurnList = rawTurn.split(" ").toList
      for {
        theirRawTurn <- rawTurnList.lift(0).flatMap(c => code.get(c))
        myRawTurn <- rawTurnList.lift(1).flatMap(c => code.get(c))
        theirTurn <- Turn(theirRawTurn)
        myTurn <- Turn(myRawTurn)
      } yield (theirTurn, myTurn)
  }
  .foldLeft(0) { case(totalScore, (them, me)) => totalScore + me.result(them) }
