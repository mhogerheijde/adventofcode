package net.hogerheijde.aoc.common.model

import java.lang.Math.pow
import java.lang.Math.sqrt

import scala.collection.IterableOnceOps

case class Coordinate(vertical: Int, horizontal: Int) extends Ordered[Coordinate]:
  @deprecated val x = horizontal
  @deprecated val y = vertical

  val row = vertical
  val column = horizontal

  val v = vertical
  val h = horizontal

  private val √ = sqrt

  def distance(other: Coordinate): Double = √(
    pow(vertical - other.vertical, 2.0) + pow(horizontal - other.horizontal, 2.0)
  )

  val transpose = new CoordinateTranslation(this)

  infix def diff(other: Coordinate): (Int, Int) = (other.v - this.v, other.h - this.h)
  infix def add(vector: (Int, Int)) = this.copy(vertical = this.v + vector._1, horizontal = this.h + vector._2)
  infix def add(coordinate: Coordinate) = this.copy(vertical = this.v + coordinate.v, horizontal = this.h + coordinate.h)

  override def toString: String = f"[$vertical%3s,$horizontal%3s]"
  override def compare(c: Coordinate): Int = Coordinate.rowsFirst.compare(this, c)

object Coordinate:
  private val rowsFirst = Ordering.by[Coordinate, Int](_.row).orElseBy(_.column)

  def apply(t: (Int, Int)): Coordinate = Coordinate(t._1, t._2)
  def range(c1: Coordinate, c2: Coordinate): CoordinateRange = CoordinateRange(c1, c2)

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

  def crlf = Coordinate(underlaying.v + 1, 0)
}

class CoordinateRange(start: Coordinate, end: Coordinate):
  def iterable =
    for
      v <- Range(start.v, end.v)
      h <- Range(start.h, end.h)
    yield Coordinate(v, h)

  def count(p: Coordinate => Boolean): Int = iterable.count(p)
  def map[T](p: Coordinate => T): Seq[T] = iterable.map(p)

