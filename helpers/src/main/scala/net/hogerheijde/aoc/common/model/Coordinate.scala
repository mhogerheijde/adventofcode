package net.hogerheijde.aoc.common.model

import java.lang.Math._

case class Coordinate(vertical: Int, horizontal: Int) extends Ordered[Coordinate] {
  val v = vertical
  val h = horizontal

  private val √ = sqrt(_)

  def distance(other: Coordinate): Double = √(
    pow(vertical - other.vertical, 2.0) + pow(horizontal - other.horizontal, 2.0)
  )

  val transpose = new CoordinateTranslation(this)

  override def toString: String = s"$vertical,$horizontal"
  override def compare(c: Coordinate): Int = Coordinate.ordering.compare(this, c)
}
object Coordinate {
  def apply(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)

  def range(c1: Coordinate, c2: Coordinate): Seq[Coordinate] = for {
    v <- Range(c1.v, c2.v)
    h <- Range(c1.h, c2.h)
  } yield Coordinate(v, h)

  private val ordering = Ordering.by[Coordinate, Int](_.h).orElseBy(_.v)
}

class CoordinateTranslation(underlaying: Coordinate) {

  def leftUp = Coordinate(underlaying.v - 1, underlaying.h - 1)
  def up = Coordinate(underlaying.v - 1, underlaying.h)
  def rightUp = Coordinate(underlaying.v - 1, underlaying.h + 1)

  def left = Coordinate(underlaying.v, underlaying.h - 1)
  def center = underlaying
  def right = Coordinate(underlaying.v, underlaying.h + 1)

  def leftDown = Coordinate(underlaying.v + 1, underlaying.h - 1)
  def down = Coordinate(underlaying.v + 1, underlaying.h)
  def rightDown = Coordinate(underlaying.v + 1, underlaying.h + 1)
}