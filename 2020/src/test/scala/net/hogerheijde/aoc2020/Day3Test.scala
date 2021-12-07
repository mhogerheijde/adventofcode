package net.hogerheijde.aoc2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with Matchers {
  "Day 3" should {
    "calculate trees" in {
      val slope = Day3.parse(
        """..##....... 0
          |#...#...#.. 1
          |.#....#..#. 2
          |..#.#...#.#
          |.#...##..#.
          |..#.##.....
          |.#.#.#....#
          |.#........#
          |#.##...#...
          |#...##....#
          |.#..#...#.#""".stripMargin)

      Day3.calculatePattern(1, 1, slope) should be (2)
      Day3.calculatePattern(3, 1, slope) should be (7)
      Day3.calculatePattern(5, 1, slope) should be (3)
      Day3.calculatePattern(7, 1, slope) should be (4)
      Day3.calculatePattern(1, 2, slope) should be (2)
    }
    "part2" in {
      val slope = Day3.parse(
        """..##.......
          |#...#...#..
          |.#....#..#.
          |..#.#...#.#
          |.#...##..#.
          |..#.##.....
          |.#.#.#....#
          |.#........#
          |#.##...#...
          |#...##....#
          |.#..#...#.#""".stripMargin)

      Day3.part2(slope) should be (336)
    }
  }
}
