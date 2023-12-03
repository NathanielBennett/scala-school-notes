
import scala.io.Source
import scala.util.Try

case class Knot(xPosition: Int, yPosition: Int) {

  lazy val neighbours: List[Knot] = (for{
    x <- (-1 to 1)
    y <- (-1 to 1)
    if (x, y) != (0, 0)
  } yield  Knot(xPosition + x, yPosition + y)).toList

  def move(x: Int = 0, y: Int = 0) = this.copy(xPosition = xPosition + x, yPosition + y)
}

case class Rope(tail: Knot, head: Knot) {

  def moveTail(newTail: Knot) = this.copy(tail = newTail)
  def isTouching: Boolean =
    head == tail || head.neighbours.contains(tail)

  //Only move the same way if touching non-diagonally
  def istouchingInline: Boolean =
    head.neighbours
      .filter{knot => knot.xPosition == head.xPosition || knot.yPosition ==  head.yPosition}
      .contains(tail)
}

val start = Rope(Knot(0, 0), Knot(0, 0))

sealed trait Step {

  protected def moveHead(rope: Rope): Rope
  protected def moveTail(rope: Rope): Rope
  protected def potentialDiagonals(knot: Knot): (Knot, Knot)

  def nextRope(rope: Rope): Rope = {
    val movedHead = moveHead(rope)
    if (movedHead.isTouching) movedHead
    else {
      val movedTailSameWay = moveTail(movedHead)
      if(movedTailSameWay.istouchingInline) movedTailSameWay
      else {
        val (diagonalOne, diagonalTwo) = potentialDiagonals(movedHead.tail)
        val eitherThisDiagnol = movedHead.moveTail(diagonalOne)
        if (eitherThisDiagnol.isTouching) eitherThisDiagnol else movedHead.moveTail(diagonalTwo)
      }
    }
  }
}

object Step {

  val s = """^(\S)\s(\d+)""".r

  def makeSteps(line: String): List[Step] = {
    val s(direction, moves) = line
    (0 until moves.toInt).map { _ =>
      val step: Option[Step] = Try {
        direction match {
          case "L" => Left
          case "R" => Right
          case "U" => Up
          case "D" => Down
        }
      }.toOption
      step
    }.toList.flatten
  }
}


case object Right extends Step {
  override def moveHead(rope: Rope): Rope = Rope(rope.tail, rope.head.move(x = 1))
  override def moveTail(rope: Rope): Rope = Rope(rope.tail.move(x = 1), rope.head)
  override def potentialDiagonals(knot: Knot): (Knot, Knot) =
    (Knot(knot.xPosition + 1, knot.yPosition - 1), Knot(knot.xPosition + 1, knot.yPosition + 1))
}

case object Left extends Step {
  override def moveHead(rope: Rope) = Rope(rope.tail, rope.head.move(x = -1))
  override def moveTail(rope: Rope) = Rope(rope.tail.move(x = -1), rope.head)
  override def potentialDiagonals(knot: Knot): (Knot, Knot) =
    (Knot(knot.xPosition - 1, knot.yPosition - 1), Knot(knot.xPosition - 1, knot.yPosition + 1))
}

case object Up extends Step {
  override def moveHead(rope: Rope) = Rope(rope.tail, rope.head.move(y = 1))
  override def moveTail(rope: Rope) = Rope(rope.tail.move(y = 1), rope.head)
  override def potentialDiagonals(knot: Knot): (Knot, Knot) =
    (Knot(knot.xPosition - 1, knot.yPosition + 1), Knot(knot.xPosition + 1, knot.yPosition + 1))
}

case object Down extends Step {
  override def moveHead(rope: Rope) = Rope(rope.tail, rope.head.move(y = -1))
  override def moveTail(rope: Rope) = Rope(rope.tail.move(y = 1), rope.head)
  override def potentialDiagonals(knot: Knot): (Knot, Knot) =
    (Knot(knot.xPosition - 1, knot.yPosition - 1), Knot(knot.xPosition - 1, knot.yPosition + 1))
}

val test = Source.fromFile(s"${System.getProperty("user.home")}/ropes.txt").getLines().toList
  .flatMap(l => Step.makeSteps(l))
  .foldLeft(List(start)){ case(ropes, step) =>
    val rope = ropes.headOption.getOrElse(start)
    step.nextRope(rope) :: ropes
  }
  .groupBy{ r => r.tail }
  .toList
  .size