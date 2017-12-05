package net.hogerheijde.aoc2017.day5

import scala.collection.immutable.IndexedSeq

trait State {
  def iteration: Int
}
object State {
  def apply(instructions: Int*): State = State(instructions.toIndexedSeq)
  def apply(instructions: IndexedSeq[Int]): State = Running(instructions, 0, 0)
  def apply(instructions: Map[Int, Int], pc: Int, iteration: Int): State = {
    if (instructions.contains(pc)) {
      Running(instructions, pc, iteration)
    } else {
      Ended(iteration)
    }

  }

}
case class Ended(iteration: Int) extends State
case class Running(instructions: Map[Int, Int], pc: Int, iteration: Int) extends State {
  require(instructions.contains(pc), "SEGFAULT!")

  override val toString: String = {
    instructions.keys.toIndexedSeq.sorted.map { line =>
      if (line == pc) {
        s"(${instructions(line)})"
      } else {
        s"${instructions(line)}"
      }
    }.mkString(" ") + s" | $iteration"
  }
}
object Running {
  def apply(instructions: IndexedSeq[Int], pc: Int, iteration: Int): Running = new Running(instructions.zipWithIndex.map(_.swap).toMap, pc, iteration)
}
