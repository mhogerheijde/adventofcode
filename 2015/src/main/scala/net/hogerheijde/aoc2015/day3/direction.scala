package net.hogerheijde.aoc2015.day3

trait Direction
object Direction {
  def apply(char: Char): Direction = {
    char match {
      case '^' => North
      case 'v' => South
      case '>' => East
      case '<' => West
    }
  }
}

case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction