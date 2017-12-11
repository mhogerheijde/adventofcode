package net.hogerheijde.aoc2017.day11

import scala.collection.immutable.IndexedSeq

case class Coordinate(x: Int, y: Int) {
  def distance(to: Coordinate): Int = Set(to.x - x, to.y - y, to.z - z).max

  def z: Int = -1 * (x + y)


  def north: Coordinate = copy(y = y + 1)
  def south: Coordinate = copy(y = y - 1)

  def northEast: Coordinate = copy(x = x - 1, y = y + 1)
  def southWest: Coordinate = copy(x = x + 1, y = y - 1)

  def northWest: Coordinate = copy(x = x + 1)
  def southEast: Coordinate = copy(x = x - 1)

  def neigbors: IndexedSeq[Coordinate] = IndexedSeq(
    northEast,
    north,
    northWest,
    southWest,
    south,
    southEast)


  def go(direction: Direction): Coordinate = {
    direction match {
      case North => north
      case South => south
      case NorthEast => northEast
      case SouthEast => southEast
      case NorthWest => northWest
      case SouthWest => southWest
    }
  }

  override val toString = s"Coordinate(x=$x, y=$y, z=$z)"


}

object Coordinate {
  val Center: Coordinate = Coordinate(0, 0)
}