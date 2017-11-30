package net.hogerheijde.aoc2016.days.day2.model

sealed trait Instruction
object Instruction {
  def fromChar(char: Char): Option[Instruction] = {
    char match {
      case 'U' => Some(GoUp)
      case 'D' => Some(GoDown)
      case 'L' => Some(GoLeft)
      case 'R' => Some(GoRight)
      case _ => None
    }
  }
}
case object GoUp extends Instruction
case object GoDown extends Instruction
case object GoLeft extends Instruction
case object GoRight extends Instruction
