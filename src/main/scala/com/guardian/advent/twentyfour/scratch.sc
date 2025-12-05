
val s = "234234234234278".map{ c => s"$c".toInt }.toList


val ll = s.take(s.length - 11)
val ll2 = s.slice(0, s.length - 11)

  // nax 4

val ll3 = s.slice(3, s.length - 10)

// 5
val ll4 = s.slice(5, s.length -9)

def buildBank(rawIndexedBank: List[(Int, Int)], currSlots: Int = 11, currSliceStart: Int = 0, acc: List[Int] = List.empty): List[Int] = {
  if (acc.size == 12) acc.reverse
  else {
    val slice = rawIndexedBank.slice(currSliceStart, rawIndexedBank.length - currSlots)
    val (max, maxIndex) = slice.maxBy(_._1)
    buildBank(rawIndexedBank, currSlots - 1, maxIndex + 1, max :: acc)
  }
}

val cv = buildBank(s.zipWithIndex)
cv.length



