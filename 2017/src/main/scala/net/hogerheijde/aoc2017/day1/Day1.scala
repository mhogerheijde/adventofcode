package net.hogerheijde.aoc2017.day1

import scala.io.Source

object Day1 {



  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("net/hogerheijde/aoc2017/day1.input").mkString.trim

    val result1 = part1(input)
    println(s"Day 1; part 1: $result1")
    val result2 = part2(input)
    println(s"Day 1; part 2: $result2")


  }

  def part1(input: String): Int = {
    (input + input.charAt(0)).toSeq.sliding(2).foldLeft(0) { case (total, chars) =>
      if (chars.distinct.length == 1) { total + Integer.parseInt(chars.head.toString, 10) } else { total }
    }
  }

  def part2(str: String): Int = {
    val rotated = str.substring(str.length/2) + str.substring(0, str.length/2)
    str.zip(rotated).foldLeft(0) { case (total, (char1, char2)) =>
      if (char1 == char2) { total + Integer.parseInt(char1.toString, 10) } else { total }
    }


  }


}
