package net.hogerheijde.aoc2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with Matchers {

  "Day 9" should {
    "solve example" in {
      val input = Seq(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576).map(_.toLong)

      Day9.findOffender(5, input) should be (127)

    }
  }
}
