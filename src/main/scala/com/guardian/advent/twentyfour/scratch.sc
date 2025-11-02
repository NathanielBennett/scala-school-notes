import com.guardian.advent.grid.{GridEntry, MovableBlock, Space}

val l: List[GridEntry[Char]] = List(
  MovableBlock(3,2, '['),
  MovableBlock(3,2, ']'),
  MovableBlock(4,2, '['),
  MovableBlock(3,2, ']'),
  MovableBlock(5,2, '['),
)

(Space(1,2, '.') :: l).dropWhile { case entry => entry.value != '[' }
  .reverse
  .dropWhile { case entry => entry.value != ']'}
  .reverse
