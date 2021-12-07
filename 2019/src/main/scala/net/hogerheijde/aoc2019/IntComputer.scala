package net.hogerheijde.aoc2019

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
import net.hogerheijde.aoc2019.Day2.Model

class IntComputer(program: IntComputer.Memory) {

  def calculate(noun: Int, verb: Int): IntComputer.Memory = {
    iterate(0, program.updated(1, noun).updated(2, verb))
  }
  def calculate(): IntComputer.Memory = {
    iterate(0, program)
  }

  @tailrec
  private def iterate(pc: Int, state: IntComputer.Memory): IntComputer.Memory = {
    state(pc) match {
      case 1 => {
        val newValue = state(state(pc + 1)) + state(state(pc + 2))
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState)
      }
      case 2 => {
        val newValue = state(state(pc + 1)) * state(state(pc + 2))
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState)
      }
      case 99 => state
      case _ => crash(pc, state)
    }
  }

  private def crash(pc: Int, state: IntComputer.Memory): IntComputer.Memory = {
    println(Console.RED + "The computer has crashed" + Console.RESET)

    val maxLengthNumber = state.max.toString.size
    println(s"PC: $pc")
    println(state.map { i => String.format("%1$" + maxLengthNumber + "s", i.toString)}.mkString(" ,") )
    println(((" " * maxLengthNumber + "  ") * pc) + ("^" * maxLengthNumber))

    state

  }
}

object IntComputer {
  type Memory = IndexedSeq[Int]
  def apply(program: Memory): IntComputer = new IntComputer(program)
}