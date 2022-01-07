package net.hogerheijde.aoc.common.model

import java.lang.Math._

case class Coordinate(x: Int, y: Int) {
  val √ = sqrt(_)
  def distance(other: Coordinate): Double = √(
    pow(x - other.x, 2.0) + pow(y - other.y, 2.0)
  )

  override def toString: String = s"$x,$y"
}
object Coordinate {
  def apply(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)
}