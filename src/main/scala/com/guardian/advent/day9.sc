import scala.io.Source
import scala.util.Try

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day9.txt").getLines().toList
  .flatMap{ line =>
    Try {
      """\s+""".r.split(line).map(_.toInt).toList
    }.toOption
  }

def diffs( list: List[Int], maybeLast: Option[Int] = None, acc: List[Int] = List.empty ): List[Int] = {
  (list, maybeLast) match {
    case (Nil, _) => acc.reverse
    case (head :: tail, None) => diffs(tail, Some(head) )
    case (head :: tail, Some(last)) => diffs(tail, Some(head), head - last :: acc)
  }
}

def diffHistory(curr: List[Int], acc: List[List[Int]] = List.empty ): List[List[Int]] = {
    if(curr.toSet == Set(0))  curr :: acc
    else {
      val next = diffs(curr)//.reverse
      diffHistory(next, curr::acc)
    }
}

def fillGaps(data: List[List[Int]]): Int = {
  def loop(data: List[List[Int]], maybeLast: Option[Int] = None, acc: List[List[Int]] = List.empty): List[List[Int]] =
    data match {
      case Nil => acc
      case head :: tail =>
        val nextHead = maybeLast.getOrElse(0) + head.reverse.head
        loop(tail, Some(nextHead), (head ::: List(nextHead)) :: acc)
    }
  loop(data).head.reverse.head
}

def fillGapsBack(data: List[List[Int]]): List[List[Int]] = {
  def loop(data: List[List[Int]], maybeLast: Option[Int] = None, acc: List[List[Int]] = List.empty): List[List[Int]] = {
    data match {
      case Nil => acc
      case head :: tail =>
        val lastHead = maybeLast.getOrElse(0)
        val nextHead = head.head - lastHead
        loop(tail, Some(nextHead) , (nextHead :: head) :: acc)
    }
  }
  loop(data)
}

val tot = lines.map( line => diffHistory(line) )
  .map{ sorted => fillGapsBack(sorted) }
  .map{l => l.head.head}
  .foldLeft(0) { case(a, b) => a + b}



/*
val tot = lines.map{ line => diffHistory(line) }
  .map{ sorted => fillGaps(sorted) }
  .foldLeft(0) { case (a, b) => a + b}*/
