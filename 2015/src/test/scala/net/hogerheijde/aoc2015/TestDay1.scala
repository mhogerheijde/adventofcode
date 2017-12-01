package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.day1.Day1
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestDay1 extends FlatSpec with Matchers {

  "Day 1; part 1" should "return 0" in {
    Day1.part1("(())") should be (0)
    Day1.part1("()()") should be (0)
  }

  it should "return 3" in {
    Day1.part1("(((") should be (3)
    Day1.part1("(()(()(") should be (3)
    Day1.part1("))(((((") should be (3)
  }

  it should "return -3" in {
    Day1.part1(")))") should be (-3)
    Day1.part1(")())())") should be (-3)
  }


  "Day 1; part 2" should "return None" in {
    Day1.part2("(())") should be (None)
    Day1.part2("()()") should be (None)
    Day1.part2("(((") should be (None)
    Day1.part2("(()(()(") should be (None)
  }

  it should "return 1" in {
    Day1.part2("))(((((") should contain (1)
    Day1.part2(")))") should contain (1)
    Day1.part2(")())())") should contain (1)
  }

  it should "return 3" in {
    Day1.part2("())()(") should contain (3)
  }


  it should "return 11" in {
    Day1.part2("((((())))))") should contain (11)
  }


}
