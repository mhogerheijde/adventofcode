package net.hogerheijde.aoc2017.day8

import net.hogerheijde.aoc2017.day8.Cpu.Instruction
import scala.annotation.tailrec

import net.hogerheijde.aoc2017.Day2017

object Day8 extends Day2017[State, Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 8"
  override def parse(input: String): State = {
    val instructions = input.lines.map(Instruction.fromString).toList
    State(instructions)
  }

  @tailrec
  override def part1(input: State): Int = {
    input.next match {
      case Ended(cpu, _) => cpu.getMaxValue
      case r: Running => part1(r)
    }

  }

  @tailrec
  override def part2(input: State): Int = {
    input.next match {
      case Ended(_, maxValue) => maxValue
      case r: Running => part2(r)
    }
  }
}
