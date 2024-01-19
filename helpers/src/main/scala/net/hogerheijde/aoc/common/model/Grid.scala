package net.hogerheijde.aoc.common.model

case class Grid[T](values: Map[Coordinate, T]) {

  override def toString: String =
    values
        .toSeq
        .sortBy(_._1)
        .map { case (c, v) => s"$c :: $v" }
        .mkString("\n")

  def pretty: String = {
    val sorted = values.toSeq.sortBy { case (c, _) => (c.horizontal, c.vertical) }
    val sb = new StringBuilder
    var lastY = sorted.headOption.map(_._1.horizontal).getOrElse(0)
    sorted.foreach { case (c, v) =>
      if (c.horizontal != lastY) {
        sb.append("\n")
        lastY = c.horizontal
      }
      sb.append(v.toString)
    }
    sb.toString()
  }

  def add(c: Coordinate, value: T): Grid[T] = Grid(values.updated(c, value))
}

object Grid {
  def apply[T](cells: Iterable[(Coordinate, T)]): Grid[T] = Grid(cells.toMap)
  def apply[T](cells: (Coordinate, T)*): Grid[T] = Grid(cells.toMap)
  def empty[T] = Grid(Map.empty[Coordinate, T])
}