package net.hogerheijde.aoc2018.day1

package object day1 {
  type Model = IndexedSeq[Drift]

  sealed trait Sign
  object Sign {
    def apply(char: Char): Sign = {
      char match {
        case '+' => Plus
        case '-' => Minus
        case _ => throw new RuntimeException("Only '+' and '-' are allowed")
      }
    }
  }

  case object Minus extends Sign
  case object Plus extends Sign

  case class Drift(sign: Sign, unsigned: Int) {
    val signed = sign match {
      case Plus => unsigned
      case Minus => -1*unsigned
    }
  }
  object Drift {
    def apply(sign: Sign, unsigned: Int): Drift = new Drift(sign, Math.abs(unsigned))
  }
}
