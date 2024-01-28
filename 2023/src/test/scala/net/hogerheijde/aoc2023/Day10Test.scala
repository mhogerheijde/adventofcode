package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.model.Grid.given
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day10.Direction.N
import net.hogerheijde.aoc2023.Day10.Direction.E
import net.hogerheijde.aoc2023.Day10.Direction.S
import net.hogerheijde.aoc2023.Day10.Direction.W
import net.hogerheijde.aoc2023.Day10.Tile
import net.hogerheijde.aoc2023.Day10.Tile.G
import net.hogerheijde.aoc2023.Day10.Tile.EW
import net.hogerheijde.aoc2023.Day10.Tile.NS
import net.hogerheijde.aoc2023.Day10.Tile.Start
import net.hogerheijde.aoc2023.Day10.Tile.NE
import net.hogerheijde.aoc2023.Day10.Tile.NW
import net.hogerheijde.aoc2023.Day10.Tile.SW
import net.hogerheijde.aoc2023.Day10.Tile.SE
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Test extends AnyWordSpec with Matchers {

  val exampleInput1: String =
    """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....
      |""".stripMargin

  val exampleInput2: String =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF
      |""".stripMargin

  val exampleInput3: String =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ
      |""".stripMargin

  val exampleGrid1: Grid[Tile] = Grid(
    (0, 0) -> G,
    (0, 1) -> G,
    (0, 2) -> G,
    (0, 3) -> G,
    (0, 4) -> G,

    (1, 0) -> G,
    (1, 1) -> Start,
    (1, 2) -> EW,
    (1, 3) -> SW,
    (1, 4) -> G,

    (2, 0) -> G,
    (2, 1) -> NS,
    (2, 2) -> G,
    (2, 3) -> NS,
    (2, 4) -> G,

    (3, 0) -> G,
    (3, 1) -> NE,
    (3, 2) -> EW,
    (3, 3) -> NW,
    (3, 4) -> G,

    (4, 0) -> G,
    (4, 1) -> G,
    (4, 2) -> G,
    (4, 3) -> G,
    (4, 4) -> G,
  )

  val exampleGrid2: Grid[Tile] = Grid(
    (0, 0) -> EW,
    (0, 1) -> NE,
    (0, 2) -> NS,
    (0, 3) -> SE,
    (0, 4) -> SW,

    (1, 0) -> SW,
    (1, 1) -> Start,
    (1, 2) -> EW,
    (1, 3) -> SW,
    (1, 4) -> NS,

    (2, 0) -> NE,
    (2, 1) -> NS,
    (2, 2) -> SW,
    (2, 3) -> NS,
    (2, 4) -> NS,

    (3, 0) -> EW,
    (3, 1) -> NE,
    (3, 2) -> EW,
    (3, 3) -> NW,
    (3, 4) -> NS,

    (4, 0) -> NE,
    (4, 1) -> NS,
    (4, 2) -> EW,
    (4, 3) -> NW,
    (4, 4) -> SE,
  )


  "Day 10 parser" should {
    "parse line" in {
      Parser.parse(Day10.line)(".....").get should be(
        Seq(
          0 -> G,
          1 -> G,
          2 -> G,
          3 -> G,
          4 -> G,
        )
      )

      Parser.parse(Day10.line)(".S-7.").get should be(
        Seq(
          0 -> G,
          1 -> Start,
          2 -> EW,
          3 -> SW,
          4 -> G,
        )
      )
    }
  }

  "Grid" should {
    "display nicecly" in {
      exampleGrid1.pretty(t => t.display) should be (
        """.....
          |.S─┐.
          |.│.│.
          |.└─┘.
          |.....""".stripMargin.replace(".", " ")
      )
    }
  }

  "Day 10" should {
    "find start" in {
      Day10.start(exampleGrid1) should be (Coordinate(1, 1))
    }

    "find next" in {
      Day10.next(exampleGrid2, Coordinate(1, 1), N) should be ((Coordinate(1, 2), W))
      Day10.next(exampleGrid2, Coordinate(1, 2), W) should be((Coordinate(1, 3), W))
      Day10.next(exampleGrid2, Coordinate(1, 3), W) should be((Coordinate(2, 3), N))
      Day10.next(exampleGrid2, Coordinate(2, 3), N) should be((Coordinate(3, 3), N))
      Day10.next(exampleGrid2, Coordinate(3, 3), N) should be((Coordinate(3, 2), E))
      Day10.next(exampleGrid2, Coordinate(3, 2), E) should be((Coordinate(3, 1), E))
      Day10.next(exampleGrid2, Coordinate(3, 1), E) should be((Coordinate(2, 1), S))
      Day10.next(exampleGrid2, Coordinate(2, 1), S) should be((Coordinate(1, 1), S))
    }

    "parse input" in {
      Day10.parse(exampleInput1) should be(exampleGrid1)
      Day10.parse(exampleInput2) should be(exampleGrid2)
    }

    "Part1: example answer" in {
      Day10.part1(Day10.parse(exampleInput1)) should be(4)
      Day10.part1(Day10.parse(exampleInput3)) should be(8)
    }

    "Part2: example answer" in {
      Day10.part2(Day10.parse(exampleInput1)) should be(0)
    }
  }
}