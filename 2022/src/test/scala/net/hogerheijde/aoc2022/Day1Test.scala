package net.hogerheijde.aoc2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Test extends AnyWordSpec with Matchers {


  val exampleInput =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin

  "Day 1" should {
    "parse input" in {
      Day1.parse(exampleInput) should be(
        Seq(
          Seq(1000, 2000, 3000),
          Seq(4000),
          Seq(5000, 6000),
          Seq(7000, 8000, 9000),
          Seq(10000),
        )
      )
    }

    "Part1: example answer" in {
      Day1.part1(Day1.parse(exampleInput)) should be (24000)
    }

    "Part2: example answer" in {
      Day1.part2(Day1.parse(exampleInput)) should be(45000)
    }


  }
}
