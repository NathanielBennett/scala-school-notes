import scala.util.Try

def retList(i: Int): List[Int] = {
  i match {
    case 0 => List(4)
    case 2 => List(4, 4, 4)
    case 4 => List(4, 4)
    case _ => Nil
  }
}

def retListEven(i: Int): List[Int] = {
  i match {
    case 1 => List(4, 4)
    case 3 => List(4, 4, 4)
    case _ => Nil
  }
}

val l = (1 to 5).map{
  _ => List(1, 2, 3).reverse
}.toList.zipWithIndex



val even = l.foldLeft(List[(List[Int], Int)]()) { case (a, b) =>
  val (list, index) = b
  val next = retList(index)
  next match {
    case Nil => a
    case _ =>
      val add = next.map { m => (m :: list, index) }
      a ::: add
  }
}

val odd = l.foldLeft(List[(List[Int], Int)]()) { case (a, b) =>
  val (list, index) = b
  val next = retListEven(index)
  next match {
    case Nil => a
    case _ =>
      val add = next.map { m => (m :: list, index) }
      a ::: add
  }
}

/*val even = l.flatMap {
  case (list, index) =>
    val next = retList(index)
    next match {
      case Nil => None
      case l => Some((l.map{ n => n :: list}, index))
    }
}*/

