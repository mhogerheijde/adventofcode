package net.hogerheijde.aoc2025

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
      |""".stripMargin

  val exampleModel: Seq[Day2.Range] =
    Seq(
      Day2.Range(11,22),
      Day2.Range(95,115),
      Day2.Range(998,1012),
      Day2.Range(1188511880,1188511890),
      Day2.Range(222220,222224),
      Day2.Range(1698522,1698528),
      Day2.Range(446443,446449),
      Day2.Range(38593856,38593862),
      Day2.Range(565653,565659),
      Day2.Range(824824821,824824827),
      Day2.Range(2121212118,2121212124),
    )

  "Day 2 parser" should {
    "parse" in {
      Parser.parse(Day2.ranges(_))(exampleInput).get should be (exampleModel)
    }
  }

  "Day 2" should {

    "know valid part1" in {
      Day2.isValidPart1(1234) should be (true)
    }
    "know invalid part1" in {
      Day2.isValidPart1(11) should be (false)
      Day2.isValidPart1(22) should be (false)
      Day2.isValidPart1(99) should be (false)
      Day2.isValidPart1(1010) should be (false)
      Day2.isValidPart1(1188511885) should be (false)
      Day2.isValidPart1(222222) should be (false)
      Day2.isValidPart1(446446) should be (false)
      Day2.isValidPart1(38593859) should be (false)
    }

    "know invalid part2 same as part1" in {
      Day2.isValidPart2(11) should be(false)
      Day2.isValidPart2(22) should be(false)
      Day2.isValidPart2(99) should be(false)
      Day2.isValidPart2(1010) should be(false)
      Day2.isValidPart2(1188511885) should be(false)
      Day2.isValidPart2(222222) should be(false)
      Day2.isValidPart2(446446) should be(false)
      Day2.isValidPart2(38593859) should be(false)
    }

    "know invalid part2 new" in {
      Day2.isValidPart2(111) should be(false)
      Day2.isValidPart2(999) should be(false)
      Day2.isValidPart2(824824824) should be(false)
      Day2.isValidPart2(565656) should be(false)

      Day2.Range(565653, 565659).filterNot(Day2.isValidPart2) should be (Seq(
        565656
      ))
    }

    "know valid part2" in {
      Day2.isValidPart2(565653) should be(true)
      Day2.isValidPart2(565654) should be(true)
      Day2.isValidPart2(565655) should be(true)
    }


    "parse input" in {
      Day2.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day2.part1(Day2.parse(exampleInput)) should be(1227775554)
    }

    "Part2: example answer" in {
      Day2.part2(Day2.parse(exampleInput)) should be(0)
    }
  }
}