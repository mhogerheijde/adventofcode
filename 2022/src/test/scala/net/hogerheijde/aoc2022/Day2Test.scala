package net.hogerheijde.aoc2022

import net.hogerheijde.aoc2022.Day2.Play
import net.hogerheijde.aoc2022.Day2.Response
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with Matchers {


  val exampleInput =
    """A Y
      |B X
      |C Z
      |""".stripMargin

  "Day 2" should {
    "parse input" in {
      Day2.parse(exampleInput) should be(
        Seq(
          (Play.Rock, Response.Y),
          (Play.Paper, Response.X),
          (Play.Scissors, Response.Z),
        )
      )
    }

    "Part1: example answer" in {
      Day2.part1(Day2.parse(exampleInput)) should be (15)
    }

    "Part2: example answer" in {
      Day2.part2(Day2.parse(exampleInput)) should be(12)
    }


  }
}
