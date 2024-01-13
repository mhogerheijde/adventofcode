package net.hogerheijde.aoc2023

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Test extends AnyWordSpec with Matchers {

  val exampleInput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  val example2Input =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  "Day 1" should {
    "parse input" in {
      Day1.parse(exampleInput) should be(Seq(
        "1abc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        "treb7uchet",
      ))
    }

    "recognise value in" in {
      Day1.valueInLine(
        "two1nine",
        Day1.digits ++ Day1.digitString,
      ) should be (29)
    }

    "Part1: example answer" in {
      Day1.part1(Day1.parse(exampleInput)) should be(142)
    }

    "Part2: example answer" in {
      Day1.part2(
        Day1.parse(example2Input)
      ) should be(281)
    }
  }
}