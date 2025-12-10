package net.hogerheijde.aoc2025

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2025.Day6.Symbol
import net.hogerheijde.aoc2025.Day6.Symbol.Operation.Add
import net.hogerheijde.aoc2025.Day6.Symbol.Operation.Multiply
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +
      |""".stripMargin

  val exampleModel: Grid[Symbol] = Grid(
    Seq(
      Coordinate(0, 0) -> Day6.Symbol.Number(123),
      Coordinate(0, 1) -> Day6.Symbol.Number(328),
      Coordinate(0, 2) -> Day6.Symbol.Number(51),
      Coordinate(0, 3) -> Day6.Symbol.Number(64),

      Coordinate(1, 0) -> Day6.Symbol.Number(45),
      Coordinate(1, 1) -> Day6.Symbol.Number(64),
      Coordinate(1, 2) -> Day6.Symbol.Number(387),
      Coordinate(1, 3) -> Day6.Symbol.Number(23),

      Coordinate(2, 0) -> Day6.Symbol.Number(6),
      Coordinate(2, 1) -> Day6.Symbol.Number(98),
      Coordinate(2, 2) -> Day6.Symbol.Number(215),
      Coordinate(2, 3) -> Day6.Symbol.Number(314),

      Coordinate(3, 0) -> Day6.Symbol.Operation.Multiply,
      Coordinate(3, 1) -> Day6.Symbol.Operation.Add,
      Coordinate(3, 2) -> Day6.Symbol.Operation.Multiply,
      Coordinate(3, 3) -> Day6.Symbol.Operation.Add,
    )
  )

  "Day 6 parser" should {
    "parse line" in {
       Parser.parse(Day6.line(_))("1  2 3   4     5").get should be (Seq(
         Day6.Symbol.Number(1),
         Day6.Symbol.Number(2),
         Day6.Symbol.Number(3),
         Day6.Symbol.Number(4),
         Day6.Symbol.Number(5)
       ))
    }
  }

  "Day 6" should {

    "parse input" in {
      Day6.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day6.part1(Day6.parse(exampleInput)) should be(4277556)
    }

    "Part2: example answer" in {
      Day6.part2(Day6.parse(exampleInput)) should be(0)
    }
  }
}