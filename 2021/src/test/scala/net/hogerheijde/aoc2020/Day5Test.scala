
package net.hogerheijde.aoc2020

import net.hogerheijde.aoc2021.Day5
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """0,9 -> 5,9
        |8,0 -> 0,8
        |9,4 -> 3,4
        |2,2 -> 2,1
        |7,0 -> 7,4
        |6,4 -> 2,0
        |0,9 -> 2,9
        |3,4 -> 1,4
        |0,0 -> 8,8
        |5,5 -> 8,2
        |""".stripMargin
    Day5.parse(input)
  }

  "Day 5 - model" should {
    "toString horivert" in {
      exampleInput.horivert.toString should be (
        """.......1..
          |..1....1..
          |..1....1..
          |.......1..
          |.112111211
          |..........
          |..........
          |..........
          |..........
          |222111....
          |""".stripMargin
      )
    }
    "toString all" in {
      println(exampleInput.toString)

      exampleInput.toString should be (
        """1.1....11.
          |.111...2..
          |..2.1.111.
          |...1.2.2..
          |.112313211
          |...1.2....
          |..1...1...
          |.1.....1..
          |1.......1.
          |222111....
          |""".stripMargin
      )
    }
  }


  "Day 5" should {
    "part 1" in {
      exampleInput.horivert.maximumOverlap(2) should be (5)
    }
    "part 2" in {
      exampleInput.maximumOverlap(2) should be(12)
    }
  }
}
