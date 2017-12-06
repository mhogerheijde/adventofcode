package net.hogerheijde.aoc2017

import scala.collection.immutable.IndexedSeq

package object day6 {
  type Register = (Int, Int)

  case class State(state: Map[Int, Int]) {
    val maxRegister: Int = state.maxBy(_._2)._2
    val size: Int = state.size
    override val toString = state.keys.toIndexedSeq.sorted.map { line => f"${state(line)}% 2d"}.mkString(" ")
  }

  object State {
    def from(ints: Int*): State = State.from(ints.toIndexedSeq)
    def from(ints: IndexedSeq[Int]): State = State(ints.zipWithIndex.map(_.swap).toMap)
  }
}
