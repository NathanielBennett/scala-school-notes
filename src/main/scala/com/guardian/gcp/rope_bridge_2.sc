import scala.util.Try

case class Position(xPosition: Int, yPosition: Int) {
  def makeNeighbour(x: Int = 0, y: Int = 0) = Position(x, y)
  lazy val neighbour: List[Position] = (for {
    y <- (-1 to 1 )
    x <- (-1 to 1 )
    if (x != xPosition && y!= yPosition)
  } yield Position(x,  y)).toList

  def isTouching(otherPosition: Position): Boolean = {
     lazy val diagonals = Seq(
       Position(xPosition - 1, yPosition + 1),
       Position(xPosition + 1, yPosition + 1),
       Position(xPosition + 1, yPosition -1),
       Position(xPosition - 1, yPosition -1)
     )

    def absOfOne(a: Int, b: Int): Boolean = Math.abs(a - b) == 1

    otherPosition == this || absOfOne(xPosition, otherPosition.yPosition) || absOfOne(yPosition, otherPosition.yPosition) || diagonals.contains(otherPosition
  }
}

case class Knot(head: Position,  tail: Position) {
   val headX = head.xPosition
   val headY = head.yPosition


  // Move to Motion
   def mayBeTailPositions(motion: Motion): Set[Position] = motion match {
     case Left(_) => Set(
       Position(this.headX -1 , this.headY),
       Position(this.headX -1 , this.headY + 1),
       Position(this.headX -1 , this.headY - 1)
     )
     case Right(_) => Set(
       Position(this.headX  + 1 , this.headY),
       Position(this.headX  + 1 , this.headY + 1),
       Position(this.headX  + 1 , this.headY - 1)
     )
     case Up(_) => Set(
       Position(this.headX, this.headY + 1),
       Position(this.headX  - 1 , this.headY + 1),
       Position(this.headX  + 1 , this.headY + 1)
     )
     case Down(_) => Set(
       Position(this.headX, this.headY - 1),
       Position(this.headX  - 1 , this.headY - 1),
       Position(this.headX  + 1 , this.headY - 1)
     )
   }
}

val start = Knot(Position(0, 0), Position(0, 0))

sealed trait Motion {
  def numberOfSteps: Int
  def makeKnot(stepNumber: Int, lastKnot: Option[Knot]): Option[Knot] = {
    val startX = lastKnot.map(_.headX).getOrElse(start.headX)
    val newHeadX = startX + stepNumber
    Knot(Position(newHeadX, 0), Position(newHeadX + 1, 0))
  }

  def knots(lastKnot: Knot): List[Knot] = {
    (0 until numberOfSteps).toList.map( stepNumber => makeKnot(stepNumber, lastKnot))
  }

  protected def mmaybeTails(movedHead: Knot): Seq[Knot]
}

object Motion {

  val m = """^(\S)\s(\d+)$""".r

  def apply(line: String): Option[Motion] = Try {
    val m(direction, moves) = line
    direction match {
      case "L" => Left(moves.toInt)
      case "R" => Right(moves.toInt)
      case "U" => Up(moves.toInt)
      case "D" => Down(moves.toInt)
    }
  }.toOption
}

case class Left(override val numberOfSteps: Int) extends Motion
case class Right(override val numberOfSteps: Int) extends Motion
case class Up(override val numberOfSteps: Int) extends Motion {
  override def makeKnot(stepNumber: Int, lastKnot: Knot):  Knot = {
    val startY = lastKnot.headX
    val newHeadY = startY + stepNumber
    Knot(Position(0, newHeadY), Position(0, newHeadY + 1))
  }
}

case class Down(override val numberOfSteps: Int) extends Motion

//val input = List("R 4", "U 4", "L 4", "D 1", "R 4")
val input = List("R 4", "U 4")//, "L 4", "D 1", "R 4")

input.map(line => Motion(line))
  .flatten
  .foldLeft(List[Knot]()) {
    case (knots, motion) =>
      val lastKnot = knots.lastOption.getOrElse(start)
      knots ::: motion.knots(lastKnot)
  }