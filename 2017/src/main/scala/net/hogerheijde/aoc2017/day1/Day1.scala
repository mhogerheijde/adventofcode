package net.hogerheijde.aoc2017.day1

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("net/hogerheijde/aoc2017/day1.input").mkString

    val result1 = part1(input)
    println(s"Day 1; part 1: $result1")


  }

  def part1(i: String): Int = {
    val input = i.trim
    (input + input.charAt(0)).toSeq.sliding(2).foldLeft(0) { case (total, chars) =>
      if (chars.distinct.length == 1) { total + Integer.parseInt(chars.head.toString, 10) } else { total }
    }
  }


}
