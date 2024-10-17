case class Entity(shape: Shape, topLeft: (Int, Int)):
  def blocks: Seq[(Int, Int)] =
    shape.pixels
      .zipWithIndex
      .flatMap((row, y) =>
        row.zipWithIndex.map((state, x) =>
          (y + topLeft._1,
            x + topLeft._2)))