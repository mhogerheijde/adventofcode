package net.hogerheijde.aoc2015

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestDay10 extends AnyWordSpec with Matchers {

  "LookAndSay" should {
    "solve examples" in {
      Day10.lookAndSay("1") should be ("11")
      Day10.lookAndSay("11") should be ("21")
      Day10.lookAndSay("21") should be ("1211")
      Day10.lookAndSay("1211") should be ("111221")
      Day10.lookAndSay("111221") should be ("312211")
    }
  }
}
