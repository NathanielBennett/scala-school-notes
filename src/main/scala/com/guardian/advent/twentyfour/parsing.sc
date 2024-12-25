type Rule = (Int, Int)

val l = List(75,47,61,53,29).permutations.toList
val l2 = List(75,47,61,53,29)
l.filter( cs => cs.size == 5).size


 def toTuples(others: List[Int], lastHead: Option[Int] = None, acc: List[(Int, Int)] = List.empty) : List[Rule] = {
  others match {
    case Nil => acc.reverse
    case head :: Nil => toTuples(Nil, Some(head), lastHead.map{ lh => (lh, head) :: acc}.getOrElse(acc))
    case head :: tail => toTuples(tail, Some(head), (tail.head, head) :: acc )
  }
}

  toTuples(l2)