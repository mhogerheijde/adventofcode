package net.hogerheijde.aoc.common.model

case class Grid[T](values: Map[Coordinate, T]) {

  override def toString: String =
    values
        .toSeq
        .sortBy(_._1)
        .map { case (c, v) => s"$c :: $v" }
        .mkString("\n")

  def pretty: String = pretty(v => v.toString)
  def pretty(toString: T => String): String = {
    val sorted = values.toSeq.sortBy { case (c, _) => (c.row, c.column) }
    val sb = new StringBuilder
    var lastRow = sorted.headOption.map(_._1.row).getOrElse(0)
    sorted.foreach { case (c, v) =>
      if (c.row != lastRow) {
        sb.append("\n")
        lastRow = c.row
      }
      sb.append(toString(v))
    }
    sb.toString()
  }

  def add(c: Coordinate, value: T): Grid[T] = Grid(values.updated(c, value))
}

object Grid {
  def apply[T](cells: Iterable[(Coordinate, T)]): Grid[T] = Grid(cells.toMap)
  def apply[T](cells: (Coordinate, T)*): Grid[T] = Grid(cells.toMap)
  def empty[T] = Grid(Map.empty[Coordinate, T])

  given tupleToCoordinate[T]: Conversion[((Int, Int), T), (Coordinate, T)] =
    case ((x: Int, y: Int), t: T) => Coordinate(x, y) -> t
}