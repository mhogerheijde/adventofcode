package net.hogerheijde.aoc2017.day11

trait Direction
object Direction {
  def fromString(direction: String): Direction = direction match {
    case "n" => North
    case "ne" => NorthEast
    case "se" => SouthEast
    case "s" => South
    case "sw" => SouthWest
    case "nw" => NorthWest
    case _ => throw new RuntimeException(s"Not a valid direction: $direction")
  }
}

case object North extends Direction
case object NorthEast extends Direction
case object SouthEast extends Direction
case object South extends Direction
case object SouthWest extends Direction
case object NorthWest extends Direction
