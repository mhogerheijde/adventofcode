package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """Time:      7  15   30
      |Distance:  9  40  200
      |""".stripMargin


  "Race" should {
    "produce options" in {
      Day6.Race(7, 9).options should be (Seq(
        Day6.Play(0, 0),
        Day6.Play(1, 6),
        Day6.Play(2, 10),
        Day6.Play(3, 12),
        Day6.Play(4, 12),
        Day6.Play(5, 10),
        Day6.Play(6, 6),
        Day6.Play(7, 0),
      ))
    }

    "find best options" in {
      Day6.Race(7, 9).betterOptions.size should be (4)
      Day6.Race(15, 40).betterOptions.size should be (8)
      Day6.Race(30, 200).betterOptions.size should be (9)
    }
  }

  "Day 6" should {

    "parse input" in {
      Day6.parse(exampleInput) should be(Seq(
        Day6.Race(7, 9),
        Day6.Race(15, 40),
        Day6.Race(30, 200),
      ))
    }

    "Part1: example answer" in {
      Day6.part1(Day6.parse(exampleInput)) should be(288)
    }

    "Part2: example answer" in {
      Day6.part2(Day6.parse(exampleInput)) should be(71503)
    }
  }
}