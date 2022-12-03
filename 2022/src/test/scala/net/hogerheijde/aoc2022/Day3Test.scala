package net.hogerheijde.aoc2022

import net.hogerheijde.aoc2022.Day3.Rucksack
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import net.hogerheijde.aoc2022.Day3.ItemHelper

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin

  val exampleModel = Seq(
    Rucksack(Seq('v', 'J', 'r', 'w', 'p', 'W', 't', 'w', 'J', 'g', 'W', 'r', 'h', 'c', 's', 'F', 'M', 'M', 'f', 'F', 'F', 'h', 'F', 'p')),
    Rucksack(
      Seq(
        'j', 'q', 'H', 'R', 'N', 'q', 'R', 'j', 'q', 'z', 'j', 'G', 'D', 'L', 'G', 'L',
        'r', 's', 'F', 'M', 'f', 'F', 'Z', 'S', 'r', 'L', 'r', 'F', 'Z', 's', 'S', 'L'
      )
    ),
    Rucksack(Seq('P', 'm', 'm', 'd', 'z', 'q', 'P', 'r', 'V', 'v', 'P', 'w', 'w', 'T', 'W', 'B', 'w', 'g')),
    Rucksack(
      Seq(
        'w', 'M', 'q', 'v', 'L', 'M', 'Z', 'H', 'h', 'H', 'M', 'v', 'w', 'L', 'H',
        'j', 'b', 'v', 'c', 'j', 'n', 'n', 'S', 'B', 'n', 'v', 'T', 'Q', 'F', 'n'
      )
    ),
    Rucksack(Seq('t', 't', 'g', 'J', 't', 'R', 'G', 'J', 'Q', 'c', 't', 'T', 'Z', 't', 'Z', 'T')),
    Rucksack(Seq('C', 'r', 'Z', 's', 'J', 's', 'P', 'P', 'Z', 's', 'G', 'z', 'w', 'w', 's', 'L', 'w', 'L', 'm', 'p', 'w', 'M', 'D', 'w')),
  )

  "Day 3" should {
    "parse input" in {
      Day3.parse(exampleInput) should be(exampleModel)
    }

    "calculate priority" in {
      'a'.priority should be (1)
      'b'.priority should be (2)
      'c'.priority should be (3)
      'd'.priority should be (4)
      'e'.priority should be (5)
      'v'.priority should be (22)
      'w'.priority should be (23)
      'x'.priority should be (24)
      'y'.priority should be (25)
      'z'.priority should be (26)
      'A'.priority should be(27)
      'B'.priority should be(28)
      'C'.priority should be(29)
      'D'.priority should be(30)
      'E'.priority should be(31)
      'V'.priority should be(48)
      'W'.priority should be(49)
      'X'.priority should be(50)
      'Y'.priority should be(51)
      'Z'.priority should be(52)

    }

    "Part1: example answer" in {
      Day3.part1(Day3.parse(exampleInput)) should be (157)
    }

    "Part2: example answer" in {
      Day3.part2(Day3.parse(exampleInput)) should be(70)
    }
  }
}
