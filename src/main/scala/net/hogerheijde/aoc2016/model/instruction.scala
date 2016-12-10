package net.hogerheijde.aoc2016.model

sealed trait Instruction {
  def amount: Int
}
case class GoLeft(amount: Int) extends Instruction
case class GoRight(amount: Int) extends Instruction

