package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  val exampleCards = Seq(
    Day4.Card(1, Set(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)),
    Day4.Card(2, Set(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)),
    Day4.Card(3, Set(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)),
    Day4.Card(4, Set(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)),
    Day4.Card(5, Set(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)),
    Day4.Card(6, Set(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11)),
  )

  "Card" should {
    "calculate part 1 points" in {
      Day4.points(Day4.Card(1, Set(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53))) should be (8)
      Day4.points(Day4.Card(2, Set(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19))) should be (2)
      Day4.points(Day4.Card(3, Set(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1))) should be (2)
      Day4.points(Day4.Card(4, Set(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83))) should be (1)
      Day4.points(Day4.Card(5, Set(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36))) should be (0)
      Day4.points(Day4.Card(6, Set(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11))) should be (0)
    }
  }

  "Day 4" should {

    "parse input" in {
      Day4.parse(exampleInput) should be(exampleCards)
    }

    "Part1: example answer" in {
      Day4.part1(Day4.parse(exampleInput)) should be(13)
    }

    "Part2: example answer" in {
      Day4.part2(Day4.parse(exampleInput)) should be(30)
    }
  }
}