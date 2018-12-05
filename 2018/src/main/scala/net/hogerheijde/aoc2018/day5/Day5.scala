package net.hogerheijde.aoc2018.day5

import net.hogerheijde.aoc2018.Day2018

object Day5 extends Day2018[String, Int, Int] {
  override def name: String = "Day 5"
  override def parse(input: String): String = input
  override def part1(input: String): Int = {



  val resultString: String = Reactor.reduce(input)
    resultString.length
  }

  override def part2(input: String): Int = {
    ('a' to 'z')
      .map(char => Reactor.reduceFiltered(input, char).length)
      .min
  }




}
