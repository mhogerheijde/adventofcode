package net.hogerheijde.aoc2016.days.day12

import net.hogerheijde.aoc2016.days.day12.RunningCpu.CpuBuilder

import scala.collection.immutable.Map
import scala.collection.immutable.IndexedSeq

case class State(registers: Map[Register, Int], instructions: IndexedSeq[Instruction], pc: Int)

trait Cpu {
  def state: State
  def step(): Cpu
}
case class HaltedCpu(state: State) extends Cpu {
  def step(): Cpu = this
}

class RunningCpu private(val state: State) extends Cpu {

  def step(): Cpu = {
    if (state.pc >= state.instructions.length) {
      HaltedCpu(state)
    } else {
      val i = state.instructions(state.pc)
      val newState: State = i match {
        case Inc(r) =>
          State(state.registers.updated(r, state.registers(r) + 1), state.instructions, state.pc + 1)
        case Dec(r) =>
          State(state.registers.updated(r, state.registers(r) - 1), state.instructions, state.pc + 1)
        case Copy(Left(source), target) =>
          State(state.registers.updated(target, source), state.instructions, state.pc + 1)
        case Copy(Right(source), target) =>
          State(state.registers.updated(target, state.registers(source)), state.instructions, state.pc + 1)
        case Nop =>
          state.copy(pc = state.pc + 1)
        case Jnz(r, jmp) =>
          state.registers(r) match {
            case 0 =>
              state.copy(pc = state.pc + 1)
            case _ =>
              state.copy(pc = state.pc + jmp)
          }
      }

      new RunningCpu(newState)
    }
  }

}
object RunningCpu {
  case class CpuBuilder(initialRegisters: Map[Register, Int] = Map.empty, initialPc: Int = 0) {
    def withRegisters(newRegisters: Map[Register, Int]): CpuBuilder = CpuBuilder(newRegisters, initialPc)
    def withPc(newPc: Int): CpuBuilder = CpuBuilder(initialRegisters, newPc)
    def load(instruction: Instruction): Cpu = new RunningCpu(State(initialRegisters.withDefaultValue(0), IndexedSeq(instruction), 0))
    def load(instructions: IndexedSeq[Instruction]): Cpu = new RunningCpu(State(initialRegisters.withDefaultValue(0), instructions, initialPc))
  }
}

object Cpu {
  def withRegisters(registers: Map[Register, Int]): CpuBuilder = CpuBuilder(registers, 0)
  def withPc(pc: Int): CpuBuilder = CpuBuilder(Map.empty, pc)
  def load(instruction: Instruction): Cpu = CpuBuilder().load(IndexedSeq(instruction))
  def load(instructions: IndexedSeq[Instruction]): Cpu = CpuBuilder().load(instructions)

}


trait Register
object Register {
  def fromString(r: String): Register = {
    r match {
      case "a" => A
      case "b" => B
      case "c" => C
      case "d" => D
      case _ => throw new IllegalArgumentException(s"No such register: $r")
    }
  }
}

case object A extends Register { override def toString = "a" }
case object B extends Register { override def toString = "b" }
case object C extends Register { override def toString = "c" }
case object D extends Register { override def toString = "d" }


trait Instruction
case class Copy(source: Either[Int, Register], target: Register) extends Instruction
case class Inc(r: Register) extends Instruction
case class Dec(r: Register) extends Instruction
case class Jnz(r: Register, jump: Int) extends Instruction
case object Nop extends Instruction // Not officially supported; added for testability
