package net.hogerheijde.aoc2025

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2025.Day3.Bank
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111
      |""".stripMargin

  val exampleModel: Seq[Bank] = Seq(
    Bank.init(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1),
    Bank.init(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9),
    Bank.init(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8),
    Bank.init(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1),
  )

  "Day 3 parser" should {
    "parse" in {
      Parser.parse(Day3.banks(_))(exampleInput).get should be (exampleModel)
    }
  }

  "Bank" should {
    "find max joltage" in {
      Bank.init(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1).maxJoltage(1) should be(9)
      Bank.init(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9).maxJoltage(1) should be(9)

      Bank.init(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1).maxJoltage(2) should be (98)
      Bank.init(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9).maxJoltage(2) should be(89)
      Bank.init(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8).maxJoltage(2) should be(78)
      Bank.init(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1).maxJoltage(2) should be(92)


      Bank.init(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1).maxJoltage(12) should be(987654321111L)
      Bank.init(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9).maxJoltage(12) should be(811111111119L)
      Bank.init(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8).maxJoltage(12) should be(434234234278L)
      Bank.init(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1).maxJoltage(12) should be(888911112111L)
    }
  }

  "Day 3" should {

    "parse input" in {
      Day3.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day3.part1(Day3.parse(exampleInput)) should be(357)
    }

    "Part2: example answer" in {
      Day3.part2(Day3.parse(exampleInput)) should be(3121910778619L)
    }
  }
}