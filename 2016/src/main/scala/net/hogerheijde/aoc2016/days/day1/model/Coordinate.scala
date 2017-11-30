package net.hogerheijde.aoc2016.days.day1.model

case class Coordinate(x: Int, y: Int) {
  def update(instruction: Direction, amount: Int): Coordinate = {
    instruction match {
      case North => Coordinate(x + amount , y)
      case South => Coordinate(x - amount , y)
      case East =>  Coordinate(x          , y + amount)
      case West =>  Coordinate(x          , y - amount)
    }
  }

  def expand(coordinate: Coordinate): IndexedSeq[Coordinate] = {
    require(coordinate.x == this.x || coordinate.y == this.y, "New coordinate must travel in a straight line to be able to expand")

    val expanded = if (coordinate.x == this.x) {
      for {
        newY <- Range.inclusive(this.y, coordinate.y, if (this.y - coordinate.y > 0) { -1 } else { 1 } )
      } yield {
        Coordinate(this.x, newY)
      }
    } else {
      for {
        newX <- Range.inclusive(this.x, coordinate.x, if (this.x - coordinate.x > 0) { -1 } else { 1 } )
      } yield {
        Coordinate(newX, this.y)
      }
    }

    expanded.tail
  }

  val distance = Math.abs(x) + Math.abs(y)

}
