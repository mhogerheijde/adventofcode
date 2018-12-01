package net.hogerheijde.aoc2018

import net.hogerheijde.aoc2018.day1.Day1

object Application {
  def main(args: Array[String]): Unit = {
    printHeader()
    Day1.run()
//    Day2.run()
//    Day3.run()
//    Day4.run()
//    Day5Stub.run()
//    Day6.run()
//    Day7.run()
//    Day8.run()
//    Day9.run()
//    Day11.run()
//    Day12.run()
//    Day13.run()
  }


  def printHeader(): Unit = {
    println("""
              |   __    ____  _  _  ____  _  _  ____    _____  ____     ___  _____  ____  ____
              |  /__\  (  _ \( \/ )( ___)( \( )(_  _)  (  _  )( ___)   / __)(  _  )(  _ \( ___)
              | /(__)\  )(_) )\  /  )__)  )  (   )(     )(_)(  )__)   ( (__  )(_)(  )(_) ))__)
              |(__)(__)(____/  \/  (____)(_)\_) (__)   (_____)(__)     \___)(_____)(____/(____)
              |
              | 2017
              |""".stripMargin)
    println()
  }
}
