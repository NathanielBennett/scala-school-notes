import scala.util.Try

def tryIt(i: Int) : Try[List[String]] = Try {
   if(i >= 0) (0 until i).toList.map(i => s"name $i")
   else throw new IllegalArgumentException("Too small")
}

def totalAllLists[S](l: List[S]): Int = {
  def totalLength(thisL: List[S], total: Int = 0): Int = {
    thisL match {
      case Nil => total
      case head :: tail => totalLength(tail, total + 1)
    }
  }
  totalLength(l)

}

for {
  thisOne <- tryIt(3)
  total = totalAllLists(thisOne)
} yield total


List() match {
  case Nil => println("empty")
  case l => print(l)

}
