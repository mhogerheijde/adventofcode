package net.hogerheijde.aoc2020

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.Common._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2020.ExitCondition.Halted
import net.hogerheijde.aoc2020.ExitCondition.Looped
import net.hogerheijde.aoc2020.Instruction.Acc
import net.hogerheijde.aoc2020.Instruction.Jump
import net.hogerheijde.aoc2020.Instruction.NoOperation

sealed trait ExitCondition {
  def acc: Long
}

object ExitCondition {
  case class Halted(acc: Long) extends ExitCondition
  case class Looped(acc: Long) extends ExitCondition
}

sealed trait Instruction {
  def amount: Int
}
object Instruction {
  case class Jump(amount: Int) extends Instruction
  case class Acc(amount: Int) extends Instruction
  case class NoOperation(amount: Int) extends Instruction
}


object Day8 extends Day[Long, Long] {
  type Model = IndexedSeq[Instruction]

  override def parse(input: String): Day8.Model = Parser.parse(instructions(_))(input).get

  override def part1(input: Day8.Model): Long = {
    run(input).acc
  }

  override def part2(input: Day8.Model): Long = {
    val mutatedPrograms = new Mutator(input)
    mutatedPrograms.map(run(_)).find {
      case Halted(_) => true
      case _ => false
    }.get.acc
  }

  class Mutator(in: Model) extends Iterator[Model] {
    private val atomicCounter = new AtomicInteger
    @tailrec
    override final def next(): Model = {
      val pc = atomicCounter.getAndIncrement()

      if (pc >= in.size) {
        throw new NoSuchElementException("Iterator has been exhausted")
      } else {
        in(pc) match {
          case Jump(amount) => in.updated(pc, NoOperation(amount))
          case NoOperation(amount) => in.updated(pc, Jump(amount))
          case Acc(_) => next()
        }
      }
    }

    override def hasNext: Boolean = in.drop(atomicCounter.get).exists {
      case NoOperation(_) | Jump(_) => true
      case Acc(_) => false
    }
  }

  def run(program: Model): ExitCondition = nextInstruction(program, 0, Seq.empty, 0)

  @tailrec
  def nextInstruction(program: Model, pc: Int, visited: Seq[Int], accumulator: Long): ExitCondition = {
    if (pc >= program.size) {
      Halted(accumulator)
    } else {
      program(pc) match {
        case Acc(amount) =>
          val nextPc = pc + 1
          nextInstruction(program, nextPc, visited :+ pc, accumulator + amount)
        case NoOperation(amount) =>
          val nextPc = pc + 1
          nextInstruction(program, nextPc, visited :+ pc, accumulator)
        case Jump(amount) =>
          val nextPc = pc + amount
          if (visited.contains(nextPc)) {
            Looped(accumulator)
          } else {
            nextInstruction(program, nextPc, visited :+ pc, accumulator)
          }
      }
    }
  }




  def acc[_: P]: P[Instruction.Acc] = P("acc " ~ int).map(Acc(_))
  def jump[_: P]: P[Jump] = P("jmp " ~ int).map(Jump(_))
  def noOperation[_: P]: P[Instruction.NoOperation] = P("nop " ~ int).map(NoOperation(_))
  def instruction[_: P]: P[Instruction] = (acc | jump | noOperation)
  def instructions[_: P]: P[IndexedSeq[Instruction]] = P(instruction ~ "\n".?).rep.map(_.toIndexedSeq)

}
