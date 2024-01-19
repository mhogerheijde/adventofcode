package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  val exampleGrid = Grid(Map(
    Coordinate(0, 0) -> Day3.Value(467),
    Coordinate(0, 5) -> Day3.Value(114),
    Coordinate(1, 3) -> Day3.Symbol('*'),
    Coordinate(2, 2) -> Day3.Value(35),
    Coordinate(2, 6) -> Day3.Value(633),
    Coordinate(3, 6) -> Day3.Symbol('#'),
    Coordinate(4, 0) -> Day3.Value(617),
    Coordinate(4, 3) -> Day3.Symbol('*'),
    Coordinate(5, 5) -> Day3.Symbol('+'),
    Coordinate(5, 7) -> Day3.Value(58),
    Coordinate(6, 2) -> Day3.Value(592),
    Coordinate(7, 6) -> Day3.Value(755),
    Coordinate(8, 3) -> Day3.Symbol('$'),
    Coordinate(8, 5) -> Day3.Symbol('*'),
    Coordinate(9, 1) -> Day3.Value(664),
    Coordinate(9, 5) -> Day3.Value(598),
  ))


  "Day 3 parser" should {
    "parse tokens" in {
      Parser.parse(Day3.token(_))("...*......") should be(Some((3, Day3.Symbol('*'))))
      Parser.parse(Day3.tokens(_))("..35..633.").get should be(
        Seq(
          (2, Day3.Value(35)),
          (6, Day3.Value(633)),
        )
      )
      Parser.parse(Day3.tokens(_))("467..114..").get should be(
        Seq(
          (0, Day3.Value(467)),
          (5, Day3.Value(114)),
        )
      )
      Parser.parse(Day3.tokens(_))("617*......").get should be(
        Seq(
          (0, Day3.Value(617)),
          (3, Day3.Symbol('*')),
        )
      )
      Parser.parse(Day3.tokens(_))("...$.*....").get should be(
        Seq(
          (3, Day3.Symbol('$')),
          (5, Day3.Symbol('*')),
        )
      )
    }

    "parse line" in {
      Parser.parse(Day3.line(_))(
        """...$.*....
          |..35..633.""".stripMargin
      ).get should be(
        Seq(
          (3, Day3.Symbol('$')),
          (5, Day3.Symbol('*')),
        )
      )
    }
  }

  "Util methods" should {
    "findAdjacent" in {
      Day3.findAdjacent(Coordinate(0, 0), Day3.Value(1)) should be (Set(
        Coordinate(-1, -1),
        Coordinate( 0, -1),
        Coordinate( 1, -1),

        Coordinate(-1,  0),
        Coordinate( 1,  0),

        Coordinate(-1,  1),
        Coordinate( 0,  1),
        Coordinate( 1,  1),
      ))

      Day3.findAdjacent(Coordinate(0, 0), Day3.Value(12)).toSeq.sorted should be(Seq(
        Coordinate(-1, -1),
        Coordinate( 0, -1),
        Coordinate( 1, -1),

        Coordinate(-1, 0),
        Coordinate( 1, 0),
        Coordinate(-1, 1),
        Coordinate( 1, 1),

        Coordinate(-1,  2),
        Coordinate( 0,  2),
        Coordinate( 1,  2),

      ).sorted)

      Day3.findAdjacent(Coordinate(0, 0), Day3.Value(123)).toSeq.sorted should be(Seq(
        Coordinate(-1, -1),
        Coordinate(0, -1),
        Coordinate(1, -1),

        Coordinate(-1, 0),
        Coordinate(1, 0),
        Coordinate(-1, 1),
        Coordinate(1, 1),
        Coordinate(-1, 2),
        Coordinate(1, 2),

        Coordinate(-1, 3),
        Coordinate(0, 3),
        Coordinate(1, 3),

      ).sorted)

      Day3.findAdjacent(Coordinate(4, 5), Day3.Value(123)).toSeq.sorted should be(Seq(
        Coordinate(3, 4),
        Coordinate(4, 4),
        Coordinate(5, 4),

        Coordinate(3, 5),
        Coordinate(5, 5),
        Coordinate(3, 6),
        Coordinate(5, 6),
        Coordinate(3, 7),
        Coordinate(5, 7),

        Coordinate(3, 8),
        Coordinate(4, 8),
        Coordinate(5, 8),

      ).sorted)
    }
  }

  "Grid" should {

    "find partnumbers" in {
      exampleGrid.partNumbers.map(_.value) should be (Seq(
        467,
        35,
        633,
        617,
        592,
        755,
        664,
        598,
      ))
    }

    "works with numbers that look like negative numbers" in {
      val exampleInput =
        """467..114..
          |...*......
          |..35..633.
          |......#...
          |617*......
          |.....+.58.
          |..592.....
          |.....-755.
          |...$......
          |.664-598..""".stripMargin

      Day3.parse(exampleInput).partNumbers.map(_.value) should be (Seq(
        467,
        35,
        633,
        617,
        592,
        755,
        664,
        598,
      ))

    }
  }

  "Day 3" should {

    "parse input" in {
      val z: Grid[Day3.Token] = Day3.parse(exampleInput)
      z should be (exampleGrid)
    }

    "Part1: example answer" in {
      Day3.part1(Day3.parse(exampleInput)) should be(4361)
    }

    "Part2: example answer" in {
      Day3.part2(Day3.parse(exampleInput)) should be(0)
    }
  }
}