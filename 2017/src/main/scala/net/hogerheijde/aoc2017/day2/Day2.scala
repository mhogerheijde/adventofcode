package net.hogerheijde.aoc2017.day2

import net.hogerheijde.aoc2017.Day2017

object Day2 extends Day2017[Sheet, Int, Int]{
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 2"

  override def parse: String => Sheet = { input =>
    Sheet(input.lines.toIndexedSeq.map(Row.fromLine))
  }

  override def part1(input: Sheet): Int = input.checksum
  override def part2(input: Sheet): Int = input.checksum2
}
