package net.hogerheijde.aoc2016

import net.hogerheijde.aoc2016.days.day1.Day1
import net.hogerheijde.aoc2016.days.day10.Day10
import net.hogerheijde.aoc2016.days.day12.Day12
import net.hogerheijde.aoc2016.days.day2.Day2
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare
import net.hogerheijde.aoc2016.days.day2.model.KeyPadStar
import net.hogerheijde.aoc2016.days.day3.Day3
import net.hogerheijde.aoc2016.days.day4.Day4
import net.hogerheijde.aoc2016.days.day5.Day5
import net.hogerheijde.aoc2016.days.day6.Day6
import net.hogerheijde.aoc2016.days.day7.Day7
import net.hogerheijde.aoc2016.days.day8.Alphabet
import net.hogerheijde.aoc2016.days.day8.Day8
import net.hogerheijde.aoc2016.days.day9.Day9

object Advent {

  def main(args: Array[String]): Unit = {
    printHeader()
    Day1.run()
    Day2.run()
    Day3.run()
    Day4.run()
//    Day5.run()
    Day6.run()
    Day7.run()
    Day9.run()
    Day8.run()
    Day9.run()
    Day10.run()
    Day12.run()
  }

  def printHeader(): Unit = {
    println("""
      |   __    ____  _  _  ____  _  _  ____    _____  ____     ___  _____  ____  ____
      |  /__\  (  _ \( \/ )( ___)( \( )(_  _)  (  _  )( ___)   / __)(  _  )(  _ \( ___)
      | /(__)\  )(_) )\  /  )__)  )  (   )(     )(_)(  )__)   ( (__  )(_)(  )(_) ))__)
      |(__)(__)(____/  \/  (____)(_)\_) (__)   (_____)(__)     \___)(_____)(____/(____)
    """.stripMargin)
    println()
  }

}
