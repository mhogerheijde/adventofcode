package net.hogerheijde.aoc2017.day8


import net.hogerheijde.aoc2017.day8.Cpu.Condition
import net.hogerheijde.aoc2017.day8.Cpu.Decrement
import net.hogerheijde.aoc2017.day8.Cpu.Increment
import net.hogerheijde.aoc2017.day8.Cpu.Instruction
import net.hogerheijde.aoc2017.day8.Cpu.Register

import scala.math.max


trait State {
  def next: State
}
object State {
  def apply(instructions: List[Instruction]): State = instructions match {
    case List () => Ended(Cpu(), 0)
    case _ => Running(Cpu(), instructions)
  }
}

case class Running(cpu: Cpu, instructions: List[Instruction], maxValue: Int = 0) extends State {
  def next: State = {
    instructions match {
      case List() =>
        Ended(cpu, max(maxValue, cpu.getMaxValue))
      case instruction :: tail =>
        val newCpu = handle(instruction)
        Running(newCpu, tail, max(maxValue, newCpu.getMaxValue))
    }
  }
  def handle(instruction: Instruction): Cpu = {
    if (conditionIsMet(instruction.condition)) {

      val register = instruction.expression.register
      val value = instruction.expression.amount
      val currentRegisterValue = cpu.registers(register)

      val newValue = instruction.expression match  {
        case _: Decrement => currentRegisterValue - value
        case _: Increment => currentRegisterValue + value
      }
      Cpu(cpu.registers.updated(register, newValue))
    } else {
      cpu
    }
  }

  def conditionIsMet(condition: Condition): Boolean = {
    val currentRegisterValue = cpu.registers(condition.register)
    condition.operator.check(currentRegisterValue)
  }
}

case class Ended(cpu: Cpu, maxValue: Int) extends State{
  def next: State = this
}

case class Cpu(registers: Map[Register, Int] = Map.empty.withDefaultValue(0) ) {
  def getMaxValue: Int = (registers.values.toSet + 0).max
}

object Cpu {
  type Register = String

  case class Instruction(expression: Expression, condition: Condition)
  object Instruction {
    def fromString(input: String): Instruction = {
      input.trim.split("if").map(_.trim).toIndexedSeq match {
        case IndexedSeq(expression, condition) => Instruction(Expression.fromString(expression), Condition.fromString(condition))
        case _ => throw new IllegalArgumentException(s"Cannot parse $input")
      }
    }
  }

  trait Expression {
    def register: Register
    def amount: Int
  }
  object Expression {
    def fromString(input: String): Expression = {
      input.trim.split(" ").toIndexedSeq.map(_.trim) match {
        case IndexedSeq(register, operator, valueStr) =>
          val value = Integer.parseInt(valueStr, 10)
          operator match {
            case "inc" => Increment(register, value)
            case "dec" => Decrement(register, value)
          }
        case _ => throw new IllegalArgumentException(s"Cannot parse $input")
      }
    }
  }

  case class Increment(register: Register, amount: Int) extends Expression
  case class Decrement(register: Register, amount: Int) extends Expression

  case class Condition(register: Register, operator: Operator)
  object Condition {
    def fromString(input: String): Condition = {
      input.trim.split(" ").toIndexedSeq.map(_.trim) match {
        case IndexedSeq(register, operator, valueStr) =>
          val value = Integer.parseInt(valueStr, 10)
          val op = operator match {
            case "<" => Smaller(value)
            case ">" => Bigger(value)
            case "==" => Equal(value)
            case "!=" => NotEqual(value)
            case ">=" => BiggerOrEqual(value)
            case "<=" => SmallerOrEqual(value)
          }

          Condition(register, op)
        case _ => throw new IllegalArgumentException(s"Cannot parse $input")
      }
    }
  }

  trait Operator {
    def check(registerValue: Int): Boolean
  }
  case class Smaller(value: Int) extends Operator {
    def check(registerValue: Int): Boolean = registerValue < value
  }
  case class Bigger(value: Int) extends Operator{
    def check(registerValue: Int): Boolean = registerValue > value
  }
  case class Equal(value: Int) extends Operator{
    def check(registerValue: Int): Boolean = registerValue == value
  }
  case class NotEqual(value: Int) extends Operator{
    def check(registerValue: Int): Boolean = registerValue != value
  }
  case class SmallerOrEqual(value: Int) extends Operator{
    def check(registerValue: Int): Boolean = registerValue <= value
  }
  case class BiggerOrEqual(value: Int) extends Operator{
    def check(registerValue: Int): Boolean = registerValue >= value
  }

}
