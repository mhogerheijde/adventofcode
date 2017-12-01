package net.hogerheijde.aoc2015.day3

case class Coordinate(vertical: Int, horizontal: Int) {
  def move(direction: Direction): Coordinate = {
    direction match {
      case North => Coordinate(vertical + 1, horizontal)
      case South => Coordinate(vertical - 1, horizontal)
      case East  => Coordinate(vertical, horizontal + 1)
      case West  => Coordinate(vertical, horizontal -1)
    }
  }
}
