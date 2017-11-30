package net.hogerheijde.aoc2016.days.day12

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.immutable.IndexedSeq

class CpuTest extends FlatSpec with Matchers {

  "Cpu" should "be initialised with zero-values for its registers" in {
    Cpu.load(IndexedSeq.empty[Instruction]).state.registers(A) should be (0)
    Cpu.load(IndexedSeq.empty[Instruction]).state.registers(B) should be (0)
    Cpu.load(IndexedSeq.empty[Instruction]).state.registers(C) should be (0)
    Cpu.load(IndexedSeq.empty[Instruction]).state.registers(D) should be (0)
  }


  it should "process inc instruction correctly" in {
    Cpu.load(Inc(A)).step().state.registers should be (Map(A -> 1))
    Cpu.load(Inc(B)).step().state.registers should be (Map(B -> 1))
    Cpu.load(Inc(C)).step().state.registers should be (Map(C -> 1))
    Cpu.load(Inc(D)).step().state.registers should be (Map(D -> 1))

    Cpu.withRegisters(Map(A -> 1)).load(Inc(A)).step().state.registers should be (Map(A -> 2))
    Cpu.withRegisters(Map(B -> 1)).load(Inc(B)).step().state.registers should be (Map(B -> 2))
    Cpu.withRegisters(Map(C -> 1)).load(Inc(C)).step().state.registers should be (Map(C -> 2))
    Cpu.withRegisters(Map(D -> 1)).load(Inc(D)).step().state.registers should be (Map(D -> 2))
  }

  it should "process dec instruction correctly" in {
    Cpu.load(Dec(A)).step().state.registers should be (Map(A -> -1))
    Cpu.load(Dec(B)).step().state.registers should be (Map(B -> -1))
    Cpu.load(Dec(C)).step().state.registers should be (Map(C -> -1))
    Cpu.load(Dec(D)).step().state.registers should be (Map(D -> -1))

    Cpu.withRegisters(Map(A -> 1)).load(Dec(A)).step().state.registers should be (Map(A -> 0))
    Cpu.withRegisters(Map(B -> 1)).load(Dec(B)).step().state.registers should be (Map(B -> 0))
    Cpu.withRegisters(Map(C -> 1)).load(Dec(C)).step().state.registers should be (Map(C -> 0))
    Cpu.withRegisters(Map(D -> 1)).load(Dec(D)).step().state.registers should be (Map(D -> 0))
  }

  it should "copy literal values into registers" in {
    Cpu.load(Copy(Left(42), A)).step().state.registers should be (Map(A -> 42))
    Cpu.load(Copy(Left(42), B)).step().state.registers should be (Map(B -> 42))
    Cpu.load(Copy(Left(42), C)).step().state.registers should be (Map(C -> 42))
    Cpu.load(Copy(Left(42), D)).step().state.registers should be (Map(D -> 42))
  }

  it should "copy register values into registers" in {
    Cpu.withRegisters(Map(D -> 42)).load(Copy(Right(D), A)).step().state.registers should be (Map(A -> 42, D -> 42))
    Cpu.withRegisters(Map(A -> 42)).load(Copy(Right(A), B)).step().state.registers should be (Map(B -> 42, A -> 42))
    Cpu.withRegisters(Map(B -> 42)).load(Copy(Right(B), C)).step().state.registers should be (Map(C -> 42, B -> 42))
    Cpu.withRegisters(Map(C -> 42)).load(Copy(Right(C), D)).step().state.registers should be (Map(D -> 42, C -> 42))
  }

  it should "jump to correct PC when register is not zero" in {
    val next1 = Cpu.withRegisters(Map(A -> 1)).load(IndexedSeq(
      Jnz(Right(A), 2),
      Copy(Left(42), A)
    )).step()

    next1.state.pc should be (2)

    val cpu = Cpu.withRegisters(Map(A -> 1)).withPc(1)
    val next2 = cpu.load(IndexedSeq(
      Nop,
      Jnz(Right(A), 3)
    )).step()

    next2.state.pc should be (4)

    val next3 = Cpu.withRegisters(Map(A -> 1)).withPc(2).load(IndexedSeq(
      Nop,
      Nop,
      Jnz(Right(A), -1)
    )).step()

    next3.state.pc should be (1)

  }


  it should "jump to correct PC when register is zero" in {
    val next1 = Cpu.load(IndexedSeq(
      Jnz(Right(A), 2),
      Nop
    )).step()

    next1.state.pc should be (1)

    val cpu = Cpu.withPc(1)
    val next2 = cpu.load(IndexedSeq(
      Nop,
      Jnz(Right(A), 3)
    )).step()

    next2.state.pc should be (2)

    val next3 = Cpu.withPc(2).load(IndexedSeq(
      Nop,
      Nop,
      Jnz(Right(A), -1)
    )).step()

    next3.state.pc should be (3)

  }

  it should "return a halted CPU when going past the last instruction" in  {
    val next = Cpu.load(IndexedSeq(
      Nop
    )).step().step()

    next should be (an[HaltedCpu])
  }

  it should "return a halted CPU when jumping past the end" in {
    val next = Cpu.withRegisters(Map(A -> 1)).withPc(2).load(IndexedSeq(
      Nop,
      Nop,
      Jnz(Right(A), 5),
      Nop,
      Nop
    )).step().step()

    next should be (an[HaltedCpu])
  }

}
