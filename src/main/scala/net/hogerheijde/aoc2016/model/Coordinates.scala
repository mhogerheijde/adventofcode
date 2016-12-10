package net.hogerheijde.aoc2016.model

case class Coordinates(x: Int, y: Int) {
  def update(instruction: Direction, amount: Int): Coordinates = {
    instruction match {
      case North => Coordinates(x + amount , y)
      case South => Coordinates(x - amount , y)
      case East =>  Coordinates(x          , y + amount)
      case West =>  Coordinates(x          , y - amount)
    }
  }

  val distance = Math.abs(x) + Math.abs(y)

}
