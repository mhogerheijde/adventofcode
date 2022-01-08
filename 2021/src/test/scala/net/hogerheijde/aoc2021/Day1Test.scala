package net.hogerheijde.aoc2021

import net.hogerheijde.aoc2021.Day1
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """199
        |200
        |208
        |210
        |200
        |207
        |240
        |269
        |260
        |263
        |""".stripMargin
    Day1.parse(input)
  }

  "Day 1" should {
    "calc part 1" in {
      Day1.part1(exampleInput) should be (7)
    }
    "calc part 2" in {
      Day1.part2(exampleInput) should be(5)
    }
  }



}
