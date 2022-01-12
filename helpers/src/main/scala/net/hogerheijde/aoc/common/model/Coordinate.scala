package net.hogerheijde.aoc.common.model

import java.lang.Math._

case class Coordinate(x: Int, y: Int) {
  val √ = sqrt(_)
  def distance(other: Coordinate): Double = √(
    pow(x - other.x, 2.0) + pow(y - other.y, 2.0)
  )

  val transpose = new CoordinateTranslation(this)
  override def toString: String = s"$x,$y"
}
object Coordinate {
  def apply(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)
}

class CoordinateTranslation(underlaying: Coordinate) {

  def leftUp = Coordinate(underlaying.x - 1, underlaying.y - 1)
  def up = Coordinate(underlaying.x - 1, underlaying.y)
  def rightUp = Coordinate(underlaying.x - 1, underlaying.y + 1)

  def left = Coordinate(underlaying.x, underlaying.y - 1)
  def center = underlaying
  def right = Coordinate(underlaying.x, underlaying.y + 1)

  def leftDown = Coordinate(underlaying.x + 1, underlaying.y - 1)
  def down = Coordinate(underlaying.x + 1, underlaying.y)
  def rightDown = Coordinate(underlaying.x + 1, underlaying.y + 1)
}