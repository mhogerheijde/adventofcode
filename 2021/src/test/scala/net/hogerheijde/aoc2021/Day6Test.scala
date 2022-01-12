
package net.hogerheijde.aoc2021

import net.hogerheijde.aoc2021.Day6
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input = "3,4,3,1,2"
    Day6.parse(input)
  }

  "Day 6 performance" should {
    "know single 1 fish iterations" in  {
      Day6.calculateDays(Seq(1), 1) should be (Seq(0).length)
      Day6.calculateDays(Seq(1), 2) should be (Seq(6, 8).length)
      Day6.calculateDays(Seq(1), 3) should be (Seq(5, 7).length)
      Day6.calculateDays(Seq(1), 4) should be (Seq(4, 6).length)
      Day6.calculateDays(Seq(1), 5) should be (Seq(3, 5).length)
      Day6.calculateDays(Seq(1), 6) should be (Seq(2, 4).length)
      Day6.calculateDays(Seq(1), 7) should be (Seq(1, 3).length)
      Day6.calculateDays(Seq(1), 8) should be (Seq(0, 2).length)
      Day6.calculateDays(Seq(1), 9) should be (Seq(6, 1, 8).length)
      Day6.calculateDays(Seq(1), 10) should be (Seq(5, 0, 7).length)
    }

    "know single 6 fish iterations" in {
      Day6.calculateDays(Seq(6), 1) should be(Seq(5).length)
      Day6.calculateDays(Seq(6), 2) should be(Seq(4).length)
      Day6.calculateDays(Seq(6), 3) should be(Seq(3).length)
      Day6.calculateDays(Seq(6), 4) should be(Seq(2).length)
      Day6.calculateDays(Seq(6), 5) should be(Seq(1).length)
      Day6.calculateDays(Seq(6), 6) should be(Seq(0).length)
      Day6.calculateDays(Seq(6), 7) should be(Seq(6, 8).length)
      Day6.calculateDays(Seq(6), 8) should be(Seq(5, 7).length)
      Day6.calculateDays(Seq(6), 9) should be(Seq(4, 6).length)
      Day6.calculateDays(Seq(6), 10) should be(Seq(3, 5).length)
      Day6.calculateDays(Seq(6), 11) should be(Seq(2, 4).length)
      Day6.calculateDays(Seq(6), 12) should be(Seq(1, 3).length)
      Day6.calculateDays(Seq(6), 13) should be(Seq(0, 2).length)
      Day6.calculateDays(Seq(6), 14) should be(Seq(6, 1, 8).length)
      Day6.calculateDays(Seq(6), 15) should be(Seq(5, 0, 7).length)
      Day6.calculateDays(Seq(6), 16) should be(Seq(4, 6, 6, 8).length)
      Day6.calculateDays(Seq(6), 17) should be(Seq(3, 5, 5, 7).length)
      Day6.calculateDays(Seq(6), 18) should be(Seq(2, 4, 4, 6).length)
      Day6.calculateDays(Seq(6), 19) should be(Seq(1, 3, 3, 5).length)
      Day6.calculateDays(Seq(6), 20) should be(Seq(0, 2, 2, 4).length)
      Day6.calculateDays(Seq(6), 21) should be(Seq(6, 1, 1, 3, 8).length)
      Day6.calculateDays(Seq(6), 22) should be(Seq(5, 0, 0, 2, 7).length)
      Day6.calculateDays(Seq(6), 23) should be(Seq(4, 6, 6, 1, 6, 8, 8).length)
    }

    "do a single fish for many days" in {
      Day6.calculateDays(Seq(1), 80) should be (1401)
      Day6.calculateDays(Seq(1), 256) should be (6206821033L)
      Day6.calculateDays(Seq(1), 300) should be (285853950736L)
    }

  }

  "Day 6" should {
    "part 1" in { Day6.part1(exampleInput) should be (5934) }
    "part 1 actual" in {
      val input = Seq(
        1, 1, 1, 1, 3, 1, 4, 1, 4, 1, 1, 2, 5, 2, 5, 1, 1, 1, 4, 3, 1, 4, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 4, 1, 1, 1, 1,
        1, 1, 1, 3, 1, 1, 5, 1, 1, 2, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 4, 3, 1, 1, 1, 2, 1, 1, 5, 2, 1, 1, 1, 1, 4, 5, 1,
        1, 2, 4, 1, 1, 1, 5, 1, 1, 1, 1, 5, 1, 3, 1, 1, 4, 2, 1, 5, 1, 2, 1, 1, 1, 1, 1, 3, 3, 1, 5, 1, 1, 1, 1, 3, 1,
        1, 1, 4, 1, 1, 1, 4, 1, 4, 3, 1, 1, 1, 4, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 5, 1, 1, 3, 5, 1, 1, 5, 2, 1, 1, 1, 1,
        1, 4, 4, 1, 1, 2, 1, 1, 1, 4, 1, 1, 1, 1, 5, 3, 1, 1, 1, 5, 1, 1, 1, 4, 1, 4, 1, 1, 1, 5, 1, 1, 3, 2, 2, 1, 1,
        1, 4, 1, 3, 1, 1, 1, 2, 1, 3, 1, 1, 1, 1, 4, 1, 1, 1, 1, 2, 1, 4, 1, 1, 1, 1, 1, 4, 1, 1, 2, 4, 2, 1, 2, 3, 1,
        3, 1, 1, 2, 1, 1, 1, 3, 1, 1, 3, 1, 1, 4, 1, 3, 1, 1, 2, 1, 1, 1, 4, 1, 1, 3, 1, 1, 5, 1, 1, 3, 1, 1, 1, 1, 5,
        1, 1, 1, 1, 1, 2, 3, 4, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 3, 2, 2, 1, 3, 5, 1, 1, 4, 4, 1, 3, 4, 1, 2, 4,
        1, 1, 3, 1
      )
      Day6.part1(input) should be (388419)
    }
    "part 2" in { Day6.part2(exampleInput) should be (26984457539L) }
  }
}
