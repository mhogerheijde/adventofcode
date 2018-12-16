package net.hogerheijde.aoc2018.day16

import scala.collection.immutable.IndexedSeq

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc2018.day16.Program.Example
import net.hogerheijde.aoc2018.day16.Program.Input
import net.hogerheijde.aoc2018.day16.Program.Instruction
import net.hogerheijde.aoc2018.day16.Program.State
import net.hogerheijde.aoc2018.day16.Program.TestCase


object CPU {

  type Register = Int
  type Value = Int

  trait Instruction {
    def operate(s: State): State
  }
  object Instruction {
    def possibleInstructions(instruction: Program.Instruction): IndexedSeq[CPU.Instruction] = {
      val A = instruction.A
      val B = instruction.B
      val C = instruction.C
      IndexedSeq(
        AddR(A, B, C),
        AddI(A, B, C),
        MulI(A, B, C),
        MulR(A, B, C),
        BanR(A, B, C),
        BanI(A, B, C),
        BorR(A, B, C),
        BorI(A, B, C),
        SetR(A, B, C),
        SetI(A, B, C),
        Gtir(A, B, C),
        Gtri(A, B, C),
        Gtrr(A, B, C),
        Eqir(A, B, C),
        Eqri(A, B, C),
        Eqrr(A, B, C),
      )
    }
  }

  case class AddR(A: Register, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) + s.get(B))
    }
  }
  case class AddI(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) + B)
    }
  }

  case class MulR(A: Register, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) * s.get(B))
    }
  }
  case class MulI(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) * B)
    }
  }

  case class BanR(A: Register, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) & s.get(B))
    }
  }
  case class BanI(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) & B)
    }
  }

  case class BorR(A: Register, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) | s.get(B))
    }
  }
  case class BorI(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A) | B)
    }
  }

  case class SetR(A: Register, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, s.get(A))
    }
  }
  case class SetI(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, A)
    }
  }

  case class Gtir(A: Value, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (A > s.get(B)).toInt)
    }
  }

  case class Gtri(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (s.get(A) > B).toInt)
    }
  }

  case class Gtrr(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (s.get(A) > s.get(B)).toInt)
    }
  }

  case class Eqir(A: Value, B: Register, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (A == s.get(B)).toInt)
    }
  }

  case class Eqri(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (s.get(A) == B).toInt)
    }
  }

  case class Eqrr(A: Register, B: Value, C: Register) extends Instruction {
    override def operate(s: State): State = {
      s.update(C, (s.get(A) == s.get(B)).toInt)
    }
  }


  implicit class BooleanToInt(b: Boolean) {
    def toInt: Int = if (b) { 1 } else { 0 }
  }
}

object Program {

  case class Input(examples: IndexedSeq[Example], testcases: IndexedSeq[TestCase]) {
    override def toString: String = {
      examples.mkString("\n") + "\n\n" + testcases.mkString("\n")
    }
  }

  type Register = Int

  case class Instruction(opcode: Int, A: Int, B: Int, C: Int) {
    override val toString: String = s"$opcode $A $B $C"
  }
  object Instruction {
    def fromSeq(s: IndexedSeq[Int]): Instruction = Instruction(s(0), s(1), s(2), s(3))
  }

  case class State(_0: Register, _1: Register, _2: Register, _3: Register) {
    def update(r: Register, value: Int): State = {
      r match {
        case 0 => copy(_0 = value)
        case 1 => copy(_1 = value)
        case 2 => copy(_2 = value)
        case 3 => copy(_3 = value)
        case other => throw new RuntimeException(s"Register out of bounds: Tried to set Register $other to value $value.")
      }
    }

    def get(r: Register): Int = {
      r match {
        case 0 => _0
        case 1 => _1
        case 2 => _2
        case 3 => _3
        case other => throw new RuntimeException(s"Register out of bounds: Tried to get value for Register $other.")
      }
    }

    override val toString: String = s"[${_0} ${_1} ${_2} ${_3}]"
  }
  object State {
    def fromSeq(s: IndexedSeq[Int]): State = State(s(0), s(1), s(2), s(3))
  }


  case class Example(before: State, instruction: Instruction, after: State) {
    override def toString: String = {
      s"""Before: $before
         |$instruction
         |After: $after
         |""".stripMargin
    }
  }

  case class TestCase(_0: Register, _1: Register, _2: Register, _3: Register) {
    override val toString: String = s"${_0} ${_1} ${_2} ${_3}"
  }
  object TestCase {
    def fromSeq(s: IndexedSeq[Int]): TestCase = TestCase(s(0), s(1), s(2), s(3))
  }

}

object Parsers {

  def example[_: P]: P[Example] = P("Before: [" ~ intSeq ~ "]\n" ~ intSeq ~ "\nAfter:  [" ~ intSeq ~ "]\n").map {
    case (before, instruction, after) =>
      Example(State.fromSeq(before), Instruction.fromSeq(instruction), State.fromSeq(after))
  }
  def examples[_: P]: P[IndexedSeq[Example]] = P( (example ~ "\n").rep).map(_.toIndexedSeq)

  def testcases[_: P]: P[IndexedSeq[TestCase]] = P((intSeq ~ "\n").rep).map(cases => cases.toIndexedSeq.map(TestCase.fromSeq))

  def cpu[_: P]: P[Input] = P(examples ~ "\n\n" ~ testcases).map(i => Input(i._1, i._2))

}
