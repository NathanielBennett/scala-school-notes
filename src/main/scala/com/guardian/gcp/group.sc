def optToList(o: Option[String]): List[Char] = {
  val t = o.map{ s => s.toCharArray}.toList
  t.flatten
}