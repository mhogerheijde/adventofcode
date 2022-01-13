package net.hogerheijde.aoc.common.model

case class Grid[T](values: Map[Coordinate, T]) {
  override def toString: String = {
    val sorted = values.toSeq.sortBy { case (c, _) => (c.y, c.x) }
    val sb = new StringBuilder
    var lastY = sorted.headOption.map(_._1.y).getOrElse(0)
    sorted.foreach { case (c, v) =>
      if (c.y != lastY) {
        sb.append("\n")
        lastY = c.y
      }
      sb.append("%X".format(v))
    }
    sb.toString()
  }
}