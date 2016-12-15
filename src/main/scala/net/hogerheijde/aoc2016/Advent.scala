package net.hogerheijde.aoc2016

import net.hogerheijde.aoc2016.days.day1.Day1
import net.hogerheijde.aoc2016.days.day2.Day2
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare
import net.hogerheijde.aoc2016.days.day2.model.KeyPadStar
import net.hogerheijde.aoc2016.days.day3.Day3
import net.hogerheijde.aoc2016.days.day4.Day4
import net.hogerheijde.aoc2016.days.day6.Day6

object Advent {

  def main(args: Array[String]): Unit = {
    printHeader()
    runDay1()
    runDay2()
    runDay3()
    runDay4()

    runDay6()
  }


  def runDay1(): Unit = {
    val instructions = Util.readFile("net/hogerheijde/aoc2016/days/day1.input")
    val distance1 = Day1.build(instructions).run()
    println(s"Day 1 - pt1: $distance1 (expect 279)")
    val distance2 = Day1.build(instructions).runPart2()
    println(s"Day 1 - pt2: $distance2 (expect 163)")

  }

  def runDay2(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day2.input")
    val code = Day2.process(input, KeyPadSquare.Five)
    println(s"Day 2 - pt1: $code (expect 53255)")
    val code2 = Day2.process(input, KeyPadStar.Five)
    println(s"Day 2 - pt2: $code2 (expect 7423A)")
  }

  def runDay3(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day3.input")
    val triangles1 = Day3.processAsRows(input)
    println(s"Day 3 - pt1: ${triangles1.length} (expect 1050)")
    val triangles2 = Day3.processAsColumns(input)
    println(s"Day 3 - pt2: ${triangles2.length} (expect 1921)")
  }

  def runDay4(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day4.input")
    val sectorTotal = Day4.process(input)
    println(s"Day 4 - pt1: $sectorTotal (expect 173787)")
    val roomSector = Day4.findRoomSector(input, "northpole object storage")
    println(s"Day 4 - pt2: ${roomSector.mkString} (expect 548)")
  }




  def runDay6(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day6.input")

    val fixed = Day6.process(input)
    println(s"Day 6 - pt1: $fixed (expect dzqckwsd)")
    val fixed2 = Day6.process2(input)
    println(s"Day 4 - pt2: $fixed2 (expect lragovly)")
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
