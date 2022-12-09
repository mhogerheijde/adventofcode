package net.hogerheijde.aoc.common.model

case class Grid[T](values: Map[Coordinate, T]) {
  override def toString: String = {
    val sorted = values.toSeq.sortBy { case (c, _) => (c.horizontal, c.vertical) }
    val sb = new StringBuilder
    var lastY = sorted.headOption.map(_._1.horizontal).getOrElse(0)
    sorted.foreach { case (c, v) =>
      if (c.horizontal != lastY) {
        sb.append("\n")
        lastY = c.horizontal
      }
      sb.append("%X".format(v))
    }
    sb.toString()
  }
}

object Grid {
  def apply[T](cells: Iterable[(Coordinate, T)]): Grid[T] = Grid(cells.toMap)
  def apply[T](cells: (Coordinate, T)*): Grid[T] = Grid(cells.toMap)
}