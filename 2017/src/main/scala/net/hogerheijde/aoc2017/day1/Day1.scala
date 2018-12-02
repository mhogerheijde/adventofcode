package net.hogerheijde.aoc2017.day1

import net.hogerheijde.aoc2017.Day2017

object Day1 extends Day2017[String, Int, Int]{
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 1"
  override def parse(input: String): String = input

  private val BaseTen = 10

  def part1(input: String): Int = {
    (input + input.charAt(0)).toSeq.sliding(2).foldLeft(0) { case (total, chars) =>
      if (chars.distinct.length == 1) { total + Integer.parseInt(chars.head.toString, BaseTen) } else { total }
    }
  }

  def part2(str: String): Int = {
    val rotated = str.substring(str.length/2) + str.substring(0, str.length/2)
    str.zip(rotated).foldLeft(0) { case (total, (char1, char2)) =>
      if (char1 == char2) { total + Integer.parseInt(char1.toString, BaseTen) } else { total }
    }
  }

}
