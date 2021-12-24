package net.hogerheijde.aoc2020

import net.hogerheijde.aoc2021.Day2
import net.hogerheijde.aoc2021.Day3
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """00100
        |11110
        |10110
        |10111
        |10101
        |01111
        |00111
        |11100
        |10000
        |11001
        |00010
        |01010
        |""".stripMargin
    Day3.parse(input)
  }

  "Day 3" should {
    "to number" in {
      Day3.toNumber(Seq(false, false, false, false)) should be (0)
      Day3.toNumber(Seq(false, false, false, true)) should be (1)
      Day3.toNumber(Seq(false, false, true, false)) should be (2)
      Day3.toNumber(Seq(false, false, true, true)) should be (3)
      Day3.toNumber(Seq(false, true, false, false)) should be (4)
      Day3.toNumber(Seq(false, true, false, true)) should be(5)
      Day3.toNumber(Seq(false, true, true, false)) should be(6)
      Day3.toNumber(Seq(false, true, true, true)) should be(7)
      Day3.toNumber(Seq(true, false, false, false)) should be(8)

    }

    "criteria" in {
      Day3.bitCriteria(exampleInput) should be (Seq(true, false, true, true, false))
      Day3.bitCriteria(exampleInput, invert = true) should be(Seq(false, true, false, false, true))

      val input =
        """00000
          |11111
          """.stripMargin

      Day3.bitCriteria(Day3.parse(input)) should be (Seq(true, true, true, true, true))
      Day3.bitCriteria(Day3.parse(input), invert = true) should be(Seq(false, false, false, false, false))
    }


    "calc part 1" in {
      Day3.part1(exampleInput) should be (198)
    }
    "calc part 2" in {
      Day3.part2(exampleInput) should be(230)
    }
  }



}
