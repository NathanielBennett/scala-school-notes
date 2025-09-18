val s = for {
  x: Int <- (0 until 3).toList
  y: Int <- (0 until 11).toList
} yield (x, y)

