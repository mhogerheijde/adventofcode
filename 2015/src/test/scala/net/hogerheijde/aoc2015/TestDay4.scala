package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.day4.Day4
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TestDay4 extends AnyFlatSpec with Matchers {
  "Day 4" should "recognise correct values" in {
    Day4.hash("abcdef")(609043) should startWith("00000")
    Day4.hash("pqrstuv")(1048970) should startWith("00000")
    Day4.hash("pqrstuv")(1048969) should not startWith("00000")
  }
}
