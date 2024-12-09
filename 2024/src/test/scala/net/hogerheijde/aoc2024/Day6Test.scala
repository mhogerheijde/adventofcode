package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Direction.East
import net.hogerheijde.aoc.common.model.Direction.North
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day6.Field
import net.hogerheijde.aoc2024.Day6.Guard
import net.hogerheijde.aoc2024.Day6.Tile.GuardTile
import net.hogerheijde.aoc2024.Day6.Tile.Obstacle
import net.hogerheijde.aoc2024.Day6.Tile.Path
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin

  val exampleField = Parser.parse(Day6.field(_))(exampleInput).get

  "Day 6 parser" should {
    "parse" in {
      exampleField.terse should be (
        Field(
          Guard(North, Coordinate(6, 4)),
          Grid(
            Map(
              Coordinate(0, 4) -> Obstacle,
              Coordinate(1, 9) -> Obstacle,
              Coordinate(3, 2) -> Obstacle,
              Coordinate(4, 7) -> Obstacle,
              Coordinate(6, 1) -> Obstacle,
              Coordinate(7, 8) -> Obstacle,
              Coordinate(8, 0) -> Obstacle,
              Coordinate(9, 6) -> Obstacle,
            )
          ),
        )
      )
    }
  }

  "Field" should {
    "take a step" in {
      exampleField.step.pretty should be (
        """....#.....
          |....>....#
          |....X.....
          |..#.X.....
          |....X..#..
          |....X.....
          |.#..X.....
          |........#.
          |#.........
          |......#...""".stripMargin
      )

      exampleField.step.step.pretty should be(
        """....#.....
          |....XXXXv#
          |....X.....
          |..#.X.....
          |....X..#..
          |....X.....
          |.#..X.....
          |........#.
          |#.........
          |......#...""".stripMargin
      )

      exampleField.step(3).pretty should be(
        """....#.....
          |....XXXXX#
          |....X...X.
          |..#.X...X.
          |....X..#X.
          |....X...X.
          |.#..X...<.
          |........#.
          |#.........
          |......#...""".stripMargin
      )

      exampleField.step(10).pretty should be(
        """....#.....
          |....XXXXX#
          |....X...X.
          |..#.X...X.
          |..XXXXX#X.
          |..X.X.X.X.
          |.#XXXXXXX.
          |.XXXXXXv#.
          |#XXXXXX...
          |......#...""".stripMargin
      )

      exampleField.step(11).pretty should be(
        """....#.....
          |....XXXXX#
          |....X...X.
          |..#.X...X.
          |..XXXXX#X.
          |..X.X.X.X.
          |.#XXXXXXX.
          |.XXXXXXX#.
          |#XXXXXXX..
          |......#X..
          |Done. (out-of-bounds)""".stripMargin
      )
    }

    "know when repeats" in {
      val goingInCricles = Parser.parse(Day6.field(_))(
        """..........
          |..........
          |..#.......
          |>.......#.
          |..........
          |..........
          |..........
          |.#........
          |.......#..
          |..........""".stripMargin
      ).get

      goingInCricles.step(7).pretty should be (
        """..........
          |..........
          |..#.......
          |XXXXXXXX#.
          |..X....X..
          |..X....X..
          |..X....X..
          |.#XXXXXX..
          |.......#..
          |..........
          |Done. (Loop)""".stripMargin
      )

    }

    "find exit south" in {
      val almostExit = Parser.parse(Day6.field(_))(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#........
          |.......v#.
          |#.........
          |......#...""".stripMargin
      ).get

      almostExit.step.pretty should be (
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#........
          |.......X#.
          |#......X..
          |......#X..
          |Done. (out-of-bounds)""".stripMargin
      )
    }

    "find exit west" in {
      val almostExit = Parser.parse(Day6.field(_))(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#........
          |.......<#.
          |#.........
          |......#...""".stripMargin
      ).get

      almostExit.step.pretty should be(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#........
          |XXXXXXXX#.
          |#.........
          |......#...
          |Done. (out-of-bounds)""".stripMargin
      )
    }

    "find exit north" in {
      val almostExit = Parser.parse(Day6.field(_))(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#........
          |........#.
          |#.....^...
          |......#...""".stripMargin
      ).get

      almostExit.step.pretty should be(
        """....#.X...
          |......X..#
          |......X...
          |..#...X...
          |......X#..
          |......X...
          |.#....X...
          |......X.#.
          |#.....X...
          |......#...
          |Done. (out-of-bounds)""".stripMargin
      )
    }

    "find exit east" in {
      val almostExit = Parser.parse(Day6.field(_))(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#>.......
          |........#.
          |#.........
          |......#...""".stripMargin
      ).get

      almostExit.step.pretty should be(
        """....#.....
          |.........#
          |..........
          |..#.......
          |.......#..
          |..........
          |.#XXXXXXXX
          |........#.
          |#.........
          |......#...
          |Done. (out-of-bounds)""".stripMargin
      )
    }

    "count visited tiles" in {
      exampleField.step.g.count { case (_, t) => t.isInstanceOf[Path.type]} should be(5)
    }

  }

  "Day 6" should {

    "parse input" in {
      Parser.parse(Day6.field(_))(exampleInput).get.pretty should be(exampleInput)
    }

    "Part1: example answer" in {
      Day6.part1(Day6.parse(exampleInput)) should be(41)
    }

    "Part2: example answer" in {
      Day6.part2(Day6.parse(exampleInput)) should be(0)
    }
  }
}