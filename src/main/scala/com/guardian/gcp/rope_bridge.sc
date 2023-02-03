

case class Position(x: Int, y: Int) {
    private def absOfOne(a: Int, b: Int) : Boolean = Math.abs(a - b) == 1

    def allNeighbours(  a :Int, b: Int ): Boolean = (a, b) == (0, 0)
    def isDiagonal( a: Int, b: Int ): Boolean = a != x && b != y

    def neighbours(tupleFilter: (Int, Int) => Boolean = allNeighbours   ): List[Position] = (for {
        xPosition  <- (-1 to 1)
        yPosition  <- (-1 to 1)
        if tupleFilter(xPosition, yPosition)
     } yield Position( x + xPosition, y + yPosition) ).toList

    def isTouching(position: Position) : Boolean = {
      this == position || absOfOne(x, position.x) || absOfOne(y, position.x) || neighbours(isDiagonal).contains(position)
    }
}

case class Knot(head: Position, tail: Position) {
   def moveHead(newHead: Position): Knot = this.copy(head = newHead)
   def moveTail(newTail: Position): Knot = this.copy(tail = newTail)
   def isTouching: Boolean = head.isTouching(tail)
}


case class Move(steps: List[Step]) {
  def knots: List[Knot] = steps.map{s => s.nextKnot}
}

trait Step {
  def potentialTails: List[Position]
  def knot: Knot
  def nextHead: Position
  def nextKnot() : Knot  = {
    val moveHead = knot.moveHead(Position(knot.head.x - 1, knot.head.y))
    if (moveHead.isTouching) moveHead else potentialTails.
  }
}
case class Left(override val knot: Knot) extends Step {

  override def nextHead: Position = Position(knot.head.x - 1, knot.head.y)

  override def potentialTails: List[Position] = {
    val currentTail = knot.tail

    List(
      Position(currentTail.x - 1, currentTail.y),
      Position(currentTail.x - 1, currentTail.y + 1),
      Position(currentTail.x - 1, currentTail.y - 1)
    )
  }
}



