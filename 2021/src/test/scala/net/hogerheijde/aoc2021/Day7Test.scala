
package net.hogerheijde.aoc2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day7Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input = "16,1,2,0,4,2,7,1,2,14"
    Day7.parse(input)
  }


  "Day 7" should {
    "part 1" in { Day7.part1(exampleInput) should be (37) }
    "part 2" in { Day7.part2(exampleInput) should be (168) }
  }
}
