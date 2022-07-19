def get(maybe: Option[Map[String, List[String]]], key: String) : List[String] = {
  val t = (for {
    map <- maybe
    value <- map.get(key)
  } yield value).getOrElse(List.empty)
}