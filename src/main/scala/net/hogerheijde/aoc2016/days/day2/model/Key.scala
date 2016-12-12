package net.hogerheijde.aoc2016.days.day2.model

trait Key {
  def update(instruction: Instruction): Key
  def prettyString(): String
}
