val l = List(1,2,3,4,5)

def shift(list: List[Int], acc: List[Int] = List.empty): List[Int] = list match {
  case Nil => acc
  case head :: tail => shift(tail, (head :: acc) )
}

shift(l)