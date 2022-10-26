import scala.annotation.tailrec

def sumOne(list: List[Int], total: Int = 0): Int = {
  if (list.size == 0) total
  else sumOne(list.tail, total + list.head)
}

def sumTwo(list: List[Int], total: Int = 0): Int = {
   list match {
     case Nil => total
     case head :: tail => sumTwo(tail, total + head)
   }
}

def listToString(list: List[String], result: String = ""): String = {
  list match {
    case Nil => result
    case head :: tail => listToString(tail, s"${result} ${head}")
  }
}

def recuriveLength(list: List[String]) : Int =
  list match {
    case Nil => 0
    case head :: tail => 1 + recuriveLength(tail)
  }

@tailrec
def tailRecursiveLength(list: List[String], accumulator: Int = 0): Int =
  list match {
    case Nil => accumulator
    case head :: tail => tailRecursiveLength(tail, accumulator + 1)
  }


