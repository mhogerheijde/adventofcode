package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Test extends AnyWordSpec with Matchers:
  val exampleInput: String =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3
      |""".stripMargin

  "Day 1" should {

    "parse input" in {
      Day1.parse(exampleInput) should be(
        Seq(3, 4, 2, 1, 3, 3),
        Seq(4, 3, 5, 3, 9, 3),
      )
    }

    "Part1: example answer" in {
      Day1.part1(Day1.parse(exampleInput)) should be(11)
    }

    "Part2: example answer" in {
      Day1.part2(Day1.parse(exampleInput)) should be(31)
    }
  }