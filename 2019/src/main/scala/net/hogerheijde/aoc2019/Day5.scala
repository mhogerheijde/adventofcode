package net.hogerheijde.aoc2019

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc2019.IntComputer.Data

object Day5 extends Day[Int, Int] {
  override type Model = IntComputer

  override def parse(in: String): Model = {
    val programMemory = in.split(",").map(_.toInt).toIndexedSeq
    IntComputer(programMemory)
  }


  override def part1(computer: Model): Int = {
    val result = computer.calculate(Data(1))
    result._2.last
  }

  override def part2(computer: Model): Int = {
    val result = computer.calculate(Data(5))
    result._2.last
  }
}
