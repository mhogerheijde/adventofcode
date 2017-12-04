package net.hogerheijde.aoc2017.day3

import net.hogerheijde.aoc2017.Day

object Day3 extends Day[Int, Int, Int]{


  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 3"

  override def parse: String => Int = Integer.parseInt

  override def part1(input: Int): Int = {
    val c = coordinateOf(input)
    c._1 + c._2 // Manhattan distance of a coordinate
  }


  override def part2(input: Int): Int = ???

  def coordinateOf(input: Int): (Int, Int) = {
    if (input == 1) { (0,0) } else {
      (ringOf(input), positionOf(input))
    }
  }

  def positionOf(input: Int): Int = {
    val power = ringPower(input)
    val cornerOffset = (Math.pow(power, 2) - input).toInt % (power - 1)
    Math.abs(cornerOffset - ringOf(input))
  }

  def ringPower(input: Int): Int = {
    val root = Math.sqrt(input).ceil.toInt
    if (root % 2 == 0) { root + 1 } else { root }

  }

  def ringOf(input: Int): Int = {
    (ringPower(input)/ 2.0).floor.toInt
  }
}
