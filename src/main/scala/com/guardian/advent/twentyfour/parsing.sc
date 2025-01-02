val list  = List(1,2,3,4,5,6, 7, 8, 9, 10)
list.permutations.size


def uniquePermutations(l: List[Int]): Iterator[List[Int]] =
  (0 to l.length).toList.flatMap{ _  => l}.combinations(l.length)


val r = uniquePermutations(list).size
