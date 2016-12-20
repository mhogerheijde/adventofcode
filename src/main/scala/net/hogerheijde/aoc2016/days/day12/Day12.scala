package net.hogerheijde.aoc2016.days.day12

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

object Day12 extends RunnableDay {

  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day12.input")

    val result = Day12.processPt1(input)
    println(s"Day 12 - pt1: $result (expect 318020)")
    val result2 = Day12.processPt2(input)
    println(s"Day 12 - pt1: $result2 (expect 9227674)")
  }

  def processPt1(input: String): Int = {
    val cpu = Cpu.load(parse(input))
    val result = run(cpu)
    result.state.registers(A)
  }

  def processPt2(input: String): Int = {

    val cpu = Cpu.withRegisters(Map(C -> 1)).load(parse(input))
    val result = run(cpu)
    result.state.registers(A)
  }

  @tailrec
  def run(cpu: Cpu): Cpu = {
    cpu match {
      case _: RunningCpu =>
        val newCpu = cpu.step()
        run(newCpu)
      case _: HaltedCpu =>
        cpu
    }
  }



  def parse(input: String): IndexedSeq[Instruction] = input.split("\n").toIndexedSeq.map(parseLine)

  val copyRegister = """cpy ([abcd]) ([abcd])""".r
  val copyInt = """cpy (-?[0-9]+) ([abcd])""".r
  val inc = """inc ([abcd])""".r
  val dec = """dec ([abcd])""".r
  val jnzRegister = """jnz ([abcd]) (-?[0-9]+)""".r
  val jnzInt = """jnz (-?[0-9]+) (-?[0-9]+)""".r

  def parseLine(input: String): Instruction = {
    input match {
      case copyRegister(source, target) => Copy(Right(Register.fromString(source)), Register.fromString(target))
      case copyInt(source, target) => Copy(Left(source.toInt), Register.fromString(target))
      case inc(r) => Inc(Register.fromString(r))
      case dec(r) => Dec(Register.fromString(r))
      case jnzRegister(r, jmp) => Jnz(Right(Register.fromString(r)), jmp.toInt)
      case jnzInt(i, jmp) => Jnz(Left(i.toInt), jmp.toInt)
    }
  }

}
