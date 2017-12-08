package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day1.Day1
import net.hogerheijde.aoc2017.day2.Day2
import net.hogerheijde.aoc2017.day3.Day3
import net.hogerheijde.aoc2017.day4.Day4
import net.hogerheijde.aoc2017.day5.Day5
import net.hogerheijde.aoc2017.day6.Day6
import net.hogerheijde.aoc2017.day7.Day7
import net.hogerheijde.aoc2017.day8.Day8

object Application {
  def main(args: Array[String]): Unit = {
    printHeader()
    Day1.run()
    Day2.run()
    Day3.run()
    Day4.run()
    Day5.run()
    Day6.run()
    Day7.run()
    Day8.run()
  }


  def printHeader(): Unit = {
    println("""
              |   __    ____  _  _  ____  _  _  ____    _____  ____     ___  _____  ____  ____
              |  /__\  (  _ \( \/ )( ___)( \( )(_  _)  (  _  )( ___)   / __)(  _  )(  _ \( ___)
              | /(__)\  )(_) )\  /  )__)  )  (   )(     )(_)(  )__)   ( (__  )(_)(  )(_) ))__)
              |(__)(__)(____/  \/  (____)(_)\_) (__)   (_____)(__)     \___)(_____)(____/(____)
              |
              | 2015
              |""".stripMargin)
    println()
  }
}
