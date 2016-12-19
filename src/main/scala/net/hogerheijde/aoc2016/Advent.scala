package net.hogerheijde.aoc2016

import net.hogerheijde.aoc2016.days.day1.Day1
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
    runDay1()
    runDay2()
    runDay3()
    runDay4()
    runDay5()
    runDay6()
    runDay7()
    runDay9()
    runDay8()
    runDay12()
  }


  def runDay1(): Unit = {
    val instructions = Util.readFile("net/hogerheijde/aoc2016/days/day1.input")
    val distance1 = Day1.build(instructions).run()
    println(s"Day 01 - pt1: $distance1 (expect 279)")
    val distance2 = Day1.build(instructions).runPart2()
    println(s"Day 01 - pt2: $distance2 (expect 163)")

  }

  def runDay2(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day2.input")
    val code = Day2.process(input, KeyPadSquare.Five)
    println(s"Day 02 - pt1: $code (expect 53255)")
    val code2 = Day2.process(input, KeyPadStar.Five)
    println(s"Day 02 - pt2: $code2 (expect 7423A)")
  }

  def runDay3(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day3.input")
    val triangles1 = Day3.processAsRows(input)
    println(s"Day 03 - pt1: ${triangles1.length} (expect 1050)")
    val triangles2 = Day3.processAsColumns(input)
    println(s"Day 03 - pt2: ${triangles2.length} (expect 1921)")
  }

  def runDay4(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day4.input")
    val sectorTotal = Day4.process(input)
    println(s"Day 04 - pt1: $sectorTotal (expect 173787)")
    val roomSector = Day4.findRoomSector(input, "northpole object storage")
    println(s"Day 04 - pt2: ${roomSector.mkString} (expect 548)")
  }

  def runDay5(): Unit = {

//    val code = Day5.crackP("ugkcyxxp")
    val code = "d4cd2ee1"
    println(s"Day 05 - pt1: $code (expect d4cd2ee1)")

//    val code2 = Day5.crack2P("ugkcyxxp")
    val code2 = "f2c730e5"
    println(s"Day 05 - pt2: $code2 (expect f2c730e5)")
  }


  def runDay6(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day6.input")

    val fixed = Day6.process(input)
    println(s"Day 06 - pt1: $fixed (expect dzqckwsd)")
    val fixed2 = Day6.process2(input)
    println(s"Day 06 - pt2: $fixed2 (expect lragovly)")
  }

  def runDay7(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day7.input")

    val amount = Day7.processPt1(input)
    println(s"Day 07 - pt1: $amount (expect 110)")
    val amount2 = Day7.processPt2(input)
    println(s"Day 07 - pt2: $amount2 (expect 242)")
  }


  def runDay8(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day8.input")

    val screen = Day8.processPt1(input)
    println(s"Day 08 - pt1: ${screen.count} (expect 115)")
    val code = Alphabet.resolve(screen.toString)
    println(s"Day 08 - pt2: $code (expect EFEYKFRFIJ")

  }

  def runDay9(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day9.input")

    val result = Day9.processPt1(input)
    println(s"Day 09 - pt1: $result (expect 120765)")
    val result2 = Day9.processPt2(input)
    println(s"Day 09 - pt2: $result2 (expect 11658395076)")
  }


  def runDay12(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day12.input")

    val result = Day12.processPt1(input)
    println(s"Day 12 - pt1: $result (expect 318020)")
    val result2 = Day12.processPt2(input)
    println(s"Day 12 - pt1: $result2 (expect 9227674)")
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
