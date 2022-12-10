package net.hogerheijde.aoc.common.model

import java.lang.Math.pow
import java.lang.Math.sqrt

import scala.collection.IterableOnceOps

case class Coordinate(vertical: Int, horizontal: Int) extends Ordered[Coordinate] {
  val v = vertical
  val h = horizontal

  private val √ = sqrt

  def distance(other: Coordinate): Double = √(
    pow(vertical - other.vertical, 2.0) + pow(horizontal - other.horizontal, 2.0)
  )

  val transpose = new CoordinateTranslation(this)

//  def ->:

  override def toString: String = s"$vertical,$horizontal"
  override def compare(c: Coordinate): Int = Coordinate.ordering.compare(this, c)
}
object Coordinate {
  def apply(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)

  def range(c1: Coordinate, c2: Coordinate): CoordinateRange = CoordinateRange(c1, c2)

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

class CoordinateRange(start: Coordinate, end: Coordinate):
  def iterable =
    for
      v <- Range(start.v, end.v)
      h <- Range(start.h, end.h)
    yield Coordinate(v, h)

  def count(p: Coordinate => Boolean): Int = iterable.count(p)
  def map[T](p: Coordinate => T): Seq[T] = iterable.map(p)

