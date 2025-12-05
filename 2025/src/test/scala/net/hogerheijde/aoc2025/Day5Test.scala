package net.hogerheijde.aoc2025

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32
      |""".stripMargin

  val exampleModel = Day5.IMS(
    Seq(
      3L to 5L,
      10L to 14L,
      16L to 20L,
      12L to 18L,
    ),
    Seq(1L, 5L, 8L, 11L, 17L, 32L)
  )


  "Day 5 parser" should {
    "parse" in {
      Parser.parse(Day5.range(_))("1-10\n").get should be (1L to 10L)
      Parser.parse(Day5.range(_))("20-95\n").get should be (20L to 95L)
      Parser.parse(Day5.ranges(_))("0-10\n20-95\n").get should be (
        Seq(
          0L to 10,
          20L to 95,
        )
      )

      Parser.parse(Day5.ingredients(_))("1\n5\n8\n11\n17\n32\n").get should be (
        Seq(1,5,8,11,17,32)
      )
    }
  }

  "Day 5" should {

    "parse input" in {
      Day5.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day5.part1(Day5.parse(exampleInput)) should be(3)
    }

    "Part2: example answer" in {
      Day5.part2(Day5.parse(exampleInput)) should be(14)
    }
  }
}