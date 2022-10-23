package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.Day8.TokenHex
import net.hogerheijde.aoc2015.Day8.TokenLiteral
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestDay8 extends AnyFlatSpec with Matchers {

  val input =
    """""
      |"abc"
      |"aaa\"aaa"
      |"\x27"""".stripMargin

  "Day8" should "calculate example" in {
    Day8.part1(Day8.parse(input)) should be (12)
  }

  it should "calculate part 2" in {
    Day8.part2(Day8.parse(input)) should be (19)
  }

}
