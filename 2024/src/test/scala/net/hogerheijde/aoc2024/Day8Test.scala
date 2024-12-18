package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day8.Field
import net.hogerheijde.aoc2024.Day8.Tile.Antenna
import net.hogerheijde.aoc2024.Day8.antiNodes
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day8Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin

  val exampleFieldTerse: Field = Field(
    Grid(
      Coordinate(1, 8) -> Antenna("0"),
      Coordinate(2, 5) -> Antenna("0"),
      Coordinate(3, 7) -> Antenna("0"),
      Coordinate(4, 4) -> Antenna("0"),

      Coordinate(5, 6) -> Antenna("A"),
      Coordinate(8, 8) -> Antenna("A"),
      Coordinate(9, 9) -> Antenna("A"),

    )
  )

  "Day 8 parser" should {
    "parse" in {
       Parser.parse(Day8.field(_))(exampleInput).get.terse should be (
         exampleFieldTerse
       )
    }
  }
  "Field" should {
    "filter frequencies" in {
      exampleFieldTerse.availableFrequencies should be(Set("0", "A"))
    }
    "find antennae for frequency" in {
      exampleFieldTerse.antennasAt("A") should be (
        Set(
          Coordinate(5, 6) -> Antenna("A"),
          Coordinate(8, 8) -> Antenna("A"),
          Coordinate(9, 9) -> Antenna("A"),
        )
      )
      exampleFieldTerse.antennasAt("0") should be (
        Set(
          Coordinate(1, 8) -> Antenna("0"),
          Coordinate(2, 5) -> Antenna("0"),
          Coordinate(3, 7) -> Antenna("0"),
          Coordinate(4, 4) -> Antenna("0"),
        )
      )
    }
    "find antinodesss" in {
      val input =
        """T.........
          |...T......
          |.T........
          |..........
          |..........
          |..........
          |..........
          |..........
          |..........
          |..........""".stripMargin
      val field = Parser.parse(Day8.field(_))(input).get

      field.antinodesssssFor(Coordinate(0, 0), Coordinate(2, 1)).toSeq.sorted should be (
        Seq(
          Coordinate(0, 0),
          Coordinate(2, 1),
          Coordinate(4, 2),
          Coordinate(6, 3),
          Coordinate(8, 4),
        )
      )

      field.antinodesssssFor(Coordinate(0, 0), Coordinate(1, 3)).toSeq.sorted should be(
        Seq(
          Coordinate(0, 0),
          Coordinate(1, 3),
          Coordinate(2, 6),
          Coordinate(3, 9),
        )
      )

      field.antinodesssssFor(Coordinate(2, 1), Coordinate(1, 3)).toSeq.sorted should be(
        Seq(
          Coordinate(0, 5),
          Coordinate(1, 3),
          Coordinate(2, 1),
        )
      )
    }

    "all antinodesss" in {
      val input =
        """T.........
          |...T......
          |.T........
          |..........
          |..........
          |..........
          |..........
          |..........
          |..........
          |..........""".stripMargin
      val field = Parser.parse(Day8.field(_))(input).get

      field.antinodessFor(Set(Coordinate(0, 0), Coordinate(1, 3), Coordinate(2, 1))).toSeq.sorted should be (
        Seq(
          Coordinate(0, 0),
          Coordinate(0, 5),
          Coordinate(1, 3),
          Coordinate(2, 1),
          Coordinate(2, 6),
          Coordinate(3, 9),
          Coordinate(4, 2),
          Coordinate(6, 3),
          Coordinate(8, 4),
        )
      )
    }
  }

  "Antenna pair" should {
    "create anti-nodes" in {
      (Coordinate(0, 0), Coordinate(1, 1)).antiNodes should be(Set(Coordinate(-1, -1), Coordinate(2, 2)))
      (Coordinate(3, 4), Coordinate(5, 5)).antiNodes should be(Set(Coordinate(1, 3), Coordinate(7, 6)))
    }
  }
  "Antenna seq" should {
    "create anti-nodes" in {
      Set(
        Coordinate(3, 4),
        Coordinate(5, 5),
        Coordinate(4, 8),
      ).antiNodes.toSeq.sorted should be(
        Seq(
          Coordinate(1, 3),
          Coordinate(2, 0),
          Coordinate(3, 11),
          Coordinate(5, 12),
          Coordinate(6, 2),
          Coordinate(7, 6),
        ).sorted
      )
    }
  }

  "Day 8" should {

    "parse input" in {
      Day8.parse(exampleInput).grid.pretty should be(exampleInput)
    }

    "Part1: example answer" in {
      Day8.part1(Day8.parse(exampleInput)) should be(14)
    }

    "Part2: example answer" in {
      Day8.part2(Day8.parse(exampleInput)) should be(34)
    }
  }
}