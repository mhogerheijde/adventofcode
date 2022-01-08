package net.hogerheijde.aoc2021

import net.hogerheijde.aoc2021.Day2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2
        |""".stripMargin
    Day2.parse(input)
  }

  "Day 1" should {
    "calc part 1" in {
      Day2.part1(exampleInput) should be (150)
    }
    "calc part 2" in {
      Day2.part2(exampleInput) should be(900)
    }
  }



}
