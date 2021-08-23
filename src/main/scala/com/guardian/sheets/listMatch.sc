def listMatch(list: List[String]) : Unit = {
  list match {
    case head :: Nil => println("One")
    case head :: second :: Nil => println("Two")
    case head :: second :: third :: Nil => println("Three")
  }
}

listMatch(List("Hello"))
listMatch(List("Hello", "There"))
listMatch(List("Hello", "There", "Bugsy"))