val inputs = Array(1, 2, 3, 2.5   )
val weights = Array(
  Array(0.2, 0.8, -0.5, 1.0),
  Array(0.5, -0.91, 0.26, -0.5),
  Array(-0.26, -0.27, 0.17, 0.87)
)
val bias = Array(2.0, 3.0, 0.5)


val output = weights.zipWithIndex.map { case (ws, index) =>

  inputs(0) * ws(0) + inputs(1) * ws(1) + inputs(2) * ws(2) + inputs(3) * ws(3) + bias(index)
}

