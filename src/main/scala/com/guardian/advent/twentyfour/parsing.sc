val x = (3,3)
val y = (4,4)


def toTuple[A](l: List[A]): Option[(A, A)] = l match {
  case List(a, b) => Some(a, b)
  case _ => None
}

def nextPrev(a: (Int, Int), b: (Int, Int)) : (Int, Int) = {



}