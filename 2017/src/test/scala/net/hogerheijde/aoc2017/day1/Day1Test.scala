package net.hogerheijde.aoc2017.day1

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day1Test extends FlatSpec with Matchers {

  "Day 1; part 1" should "calclulate value" in {
    Day1.part1("1122") should be(3)
    Day1.part1("1111\n") should be(4)
    Day1.part1("1234") should be(0)
    Day1.part1("91212129") should be(9)
  }

}
