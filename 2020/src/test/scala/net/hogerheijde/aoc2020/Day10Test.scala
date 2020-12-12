package net.hogerheijde.aoc2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Test extends AnyWordSpec with Matchers {

  val example1 = Seq(0, 16 ,10 ,15 ,5 ,1 ,11 ,7 ,19 ,6 ,12 ,4, 22).map(_.toLong)
  val example2 = Seq(0, 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3, 52)
    .map(_.toLong)

  "Day 10" should {
    "solve example part 1" in {


      Day10.part1(example1) should be (35)



      Day10.part1(example2) should be (220)

    }

    "solve example part 2" in {
      Day10.part2(example1) should be (8)
      Day10.part2(example2) should be (19208)
    }
  }
}
