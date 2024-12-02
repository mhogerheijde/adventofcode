package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Report
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin


  "Integer method" should {
    "determine isBetween" in {
      0.isBetween(1, 3) should be (false)
      1.isBetween(1, 3) should be (true)
      2.isBetween(1, 3) should be (true)
      3.isBetween(1, 3) should be (true)
      4.isBetween(1, 3) should be (false)
    }

    "determine strictly increasing" in {
      Seq(1).strictlyIncreasing should be (true)
      Seq(1, 2).strictlyIncreasing should be (true)
      Seq(-200, 0, 100).strictlyIncreasing should be (true)
      Seq(1, 3, 6, 7, 9).strictlyIncreasing should be (true)
      Seq(1, 2, 3, 4, 5, 6).strictlyIncreasing should be (true)

      Seq(2, 1).strictlyIncreasing should be(false)
      Seq(200, 300, 100).strictlyIncreasing should be(false)
      Seq(1, 3, 6, 6, 9).strictlyIncreasing should be(false)
      Seq(4, 3, 2, 1).strictlyIncreasing should be(false)
    }

    "determine strictly decreasing" in {
      Seq(1).strictlyDecreasing should be(true)
      Seq(2, 1).strictlyDecreasing should be(true)
      Seq(100, 0, -200).strictlyDecreasing should be(true)
      Seq(9, 7, 6, 3, 1).strictlyDecreasing should be(true)
      Seq(6, 5, 4, 3, 2, 1).strictlyDecreasing should be(true)

      Seq(1, 2).strictlyDecreasing should be(false)
      Seq(200, 100, 300).strictlyDecreasing should be(false)
      Seq(9, 6, 6, 3, 1).strictlyDecreasing should be(false)
      Seq(1, 2, 3, 4).strictlyDecreasing should be(false)
    }

  }

  "Report" should {
    "determine safety" in {
      Report(Seq(7, 6, 4, 2, 1)).isSafe should be (true)
      Report(Seq(1, 2, 7, 8, 9)).isSafe should be (false)
      Report(Seq(9, 7, 6, 2, 1)).isSafe should be (false)
      Report(Seq(1, 3, 2, 4, 5)).isSafe should be (false)
      Report(Seq(8, 6, 4, 4, 1)).isSafe should be (false)
      Report(Seq(1, 3, 6, 7, 9)).isSafe should be (true)
    }
    "dampened safety" in {
      Report(Seq(7, 6, 4, 2, 1)).canBeDampened should be(true)
      Report(Seq(1, 2, 7, 8, 9)).canBeDampened should be(false)
      Report(Seq(9, 7, 6, 2, 1)).canBeDampened should be(false)
      Report(Seq(1, 3, 2, 4, 5)).canBeDampened should be(true)
      Report(Seq(8, 6, 4, 4, 1)).canBeDampened should be(true)
      Report(Seq(1, 3, 6, 7, 9)).canBeDampened should be(true)
    }
  }

  "Day 2 parser" should {
    "parse" in {
      // Parser.parse(...)("...").get should be ("")
    }
  }

  "Day 2" should {

    "parse input" in {
      Day2.parse(exampleInput) should be(
        Seq(
          Report(IndexedSeq(7, 6, 4, 2, 1)),
          Report(IndexedSeq(1, 2, 7, 8, 9)),
          Report(IndexedSeq(9, 7, 6, 2, 1)),
          Report(IndexedSeq(1, 3, 2, 4, 5)),
          Report(IndexedSeq(8, 6, 4, 4, 1)),
          Report(IndexedSeq(1, 3, 6, 7, 9)),
        )

      )
    }

    "Part1: example answer" in {
      Day2.part1(Day2.parse(exampleInput)) should be(2)
    }

    "Part2: example answer" in {
      Day2.part2(Day2.parse(exampleInput)) should be(4)
    }
  }
}