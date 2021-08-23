def getOpt(i: Int) : Option[String] = if (i % 2 == 0) Some(s"Hello $i") else None
def default: String = "Have it"

getOpt(1)
getOpt(2)

val opt = getOpt(2)
val getOr = opt.getOrElse(default)
val fold = opt.fold(default)( s => s.toUpperCase)

val l = List(1,2,3,4)

val l2 = "Paul" :: List("Jack", "Jane")

val leftToRight = l2.foldLeft(List[String]()){ case (newList, add) => s"$add 1" :: newList}
val rightToLeft = l2.foldRight(List[String]()){ case (add, newList) => s"$add 1" :: newList }
val what = l2.fold(""){ case (add, newList) => add + newList }
val what2 = l2.fold(""){ case (buildedString, newName) =>
  if(buildedString.isEmpty) buildedString + newName else s"$buildedString + " + newName
}

case class Thingy(name: String, size: Int)

val t = Thingy("alex", 4)

val name, number = t