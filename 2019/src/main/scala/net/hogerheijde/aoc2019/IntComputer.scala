package net.hogerheijde.aoc2019

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2019.Day2.Model
import net.hogerheijde.aoc2019.IntComputer.Data
import net.hogerheijde.aoc2019.IntComputer.Instruction
import net.hogerheijde.aoc2019.IntComputer.Memory
import net.hogerheijde.aoc2019.IntComputer.ProgramCounter
import net.hogerheijde.aoc2019.ParameterMode.Positional
import net.hogerheijde.aoc2019.ParameterMode.Immediate

sealed trait ParameterMode
object ParameterMode {

  object Immediate extends ParameterMode
  object Positional extends ParameterMode

  def foo: String = ""

  def fromInt(mode: Int): ParameterMode = {
    mode match {
      case 0 => Positional
      case 1 => Immediate
    }
  }
}


class IntComputer(program: Memory) {

  def calculate(inputs: Data): (Memory, Data) = {
    iterate(0, program, inputs)
  }

  def calculate(noun: Int, verb: Int): Memory = {
    iterate(0, program.updated(1, noun).updated(2, verb))._1
  }
  def calculate(): Memory = {
    iterate(0, program)._1
  }

  @tailrec
  private def iterate(pc: ProgramCounter, state: Memory, inputs: Data = Data(), outputs: Data = Data()): (Memory, Data) = {
    val instruction = state(pc)
    instruction % 100 match {
      case 1 => { //addition
        val (value1, value2) = read2(instruction, pc, state)

        val newValue = value1 + value2
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState, inputs, outputs)
      }
      case 2 => { // multiply
        val (value1, value2) = read2(instruction, pc, state)

        val newValue = value1 * value2
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState, inputs, outputs)
      }
      case 3 => { // input
        val newValue = inputs.head
        val newState = state.updated(state(pc + 1), newValue)
        iterate(pc + 2, newState, inputs.tail, outputs)
      }
      case 4 => { // output
        val value = getValue(state, state(pc + 1), readModes1(instruction))
        iterate(pc + 2, state, inputs, outputs :+ value)
      }
      case 5 => {
        // jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter.
        val (value1, value2) = read2(instruction, pc, state)
        val nextPc = if (value1 != 0) { value2 } else { pc + 3}
        iterate(nextPc, state, inputs, outputs)
      }
      case 6 => {
        // jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter.
        // Otherwise, it does nothing.
        val (value1, value2) = read2(instruction, pc, state)
        val nextPc = if (value1 == 0) { value2 } else { pc + 3}
        iterate(nextPc, state, inputs, outputs)
      }
      case 7 => {
        // less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter.
        // Otherwise, it stores 0.
        val (value1, value2) = read2(instruction, pc, state)

        val newValue = if (value1 < value2) { 1 } else { 0 }
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState, inputs, outputs)
      }
      case 8 => {
        // equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter.
        // Otherwise, it stores 0.
        val (value1, value2) = read2(instruction, pc, state)

        val newValue = if (value1 == value2) { 1 } else { 0 }
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState, inputs, outputs)
      }

      case 99 => // halt
        (state, outputs)
      case _ => // crash
        (crash(pc, state), outputs)
    }
  }


  private def read1(instruction: Instruction, pc: ProgramCounter, state: Memory): Int = getValue(state, state(pc + 1), readModes1(instruction))
  private def read2(instruction: Instruction, pc: ProgramCounter, state: Memory): (Int, Int) = {
    val (mode1, mode2) = readModes2(instruction)
    (getValue(state, state(pc + 1), mode1), getValue(state, state(pc + 2), mode2))
  }

  private def readModes1(instruction: Instruction): ParameterMode = ParameterMode.fromInt(instruction / 100 % 10)
  private def readModes2(instruction: Instruction): (ParameterMode, ParameterMode) = {
    (ParameterMode.fromInt(instruction / 100 % 10),
      ParameterMode.fromInt(instruction / 1000 % 10))
  }

  private def getValue(memory: Memory, parameter: Int, mode: ParameterMode): Int = {
    mode match {
      case Positional => memory(parameter)
      case Immediate => parameter
    }
  }



  private def crash(pc: Int, state: Memory): Memory = {
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
  val Memory = IndexedSeq
  type Data = IndexedSeq[Int]
  val Data = IndexedSeq
  type Instruction = Int
  type ProgramCounter = Int
  def apply(program: Memory): IntComputer = new IntComputer(program)
}