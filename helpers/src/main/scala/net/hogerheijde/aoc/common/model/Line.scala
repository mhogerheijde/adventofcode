package net.hogerheijde.aoc.common.model

import scala.annotation.tailrec

case class Line(
    start: Coordinate,
    end: Coordinate,
) {
  val minX = Math.min(start.vertical, end.vertical)
  val minY = Math.min(start.horizontal, end.horizontal)
  val maxX = Math.max(start.vertical, end.vertical)
  val maxY = Math.max(start.horizontal, end.horizontal)

  def collision(point: Coordinate): Boolean = {
    // Point P is on line AB if |AP| + |PB| == |AB|
    val lineLen = start.distance(end)
    val startPoint = start.distance(point)
    val endPoint = end.distance(point)
    (startPoint + endPoint) - lineLen < 1E-5
  }

  override def toString = s"$start -> $end"
}

object Line {
  def apply(start: (Int, Int), end: (Int, Int)): Line =
    Line(Coordinate(start), Coordinate(end))
  def apply(x1: Int, y1: Int, x2: Int, y2: Int): Line =
    Line(Coordinate(x1, y1), Coordinate(x2, y2))
}