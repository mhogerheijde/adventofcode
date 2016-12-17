package net.hogerheijde.aoc2016.days.day12

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

object Day12 {


  def processPt1(input: String): Int = {

    val cpu = Cpu.load(parse(input))
    val result = run(cpu)

    result.state.registers(A)
  }

  @tailrec
  def run(cpu: Cpu): Cpu = {
    cpu match {
      case _: RunningCpu =>
        cpu.step()
        run(cpu)
      case _: HaltedCpu =>
        cpu
    }
  }



  def parse(input: String): IndexedSeq[Instruction] = input.split("\n").toIndexedSeq.map(parseLine)

  val copyRegister = """cpy ([abcd]) ([abcd])""".r
  val copyInt = """cpy (-?[0-9]+) ([abcd])""".r
  val inc = """inc ([abcd])""".r
  val dec = """dec ([abcd])""".r
  val jnz = """jnz ([abcd]) (-?[0-9]+)""".r

  def parseLine(input: String): Instruction = {
    input match {
      case copyRegister(source, target) => Copy(Right(Register.fromString(source)), Register.fromString(target))
      case copyInt(source, target) => Copy(Left(source.toInt), Register.fromString(target))
      case inc(r) => Inc(Register.fromString(r))
      case dec(r) => Dec(Register.fromString(r))
      case jnz(r, jmp) => Jnz(Register.fromString(r), jmp.toInt)
    }
  }

}
