package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Direction.East
import net.hogerheijde.aoc.common.model.Direction.North
import net.hogerheijde.aoc.common.model.Direction.SouthEast
import net.hogerheijde.aoc.common.model.Direction.West
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day4.xmas
import net.hogerheijde.aoc2024.Day4.`x-mas`
import net.hogerheijde.aoc2024.Day4.countXmas
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX
      |""".stripMargin

  val exampleModel = Day4.parse(exampleInput)

  "Day 4 parser" should {
    "parse" in {
      // Parser.parse(...)("...").get should be ("")
    }
  }

  "Day 4" should {


    "find XMAS for coordinate in east direction" in {
      exampleModel.xmas(Coordinate(0, 0), East) should be(false)
      exampleModel.xmas(Coordinate(0, 1), East) should be(false)
      exampleModel.xmas(Coordinate(0, 2), East) should be(false)
      exampleModel.xmas(Coordinate(0, 3), East) should be(false)
      exampleModel.xmas(Coordinate(0, 4), East) should be(false)
      exampleModel.xmas(Coordinate(0, 5), East) should be (true)

      exampleModel.xmas(Coordinate(0, 4), SouthEast) should be (true)

      exampleModel.xmas(Coordinate(1, 4), West) should be (true)
      exampleModel.xmas(Coordinate(4, 6), West) should be (true)
      exampleModel.xmas(Coordinate(4, 6), North) should be (true)
    }

    "find X-MAS for coordinate in east direction" in {

      exampleModel.`x-mas`(Coordinate(0, 2)) should be(false)
      exampleModel.`x-mas`(Coordinate(0, 1)) should be(false)
      exampleModel.`x-mas`(Coordinate(1, 1)) should be(false)
      exampleModel.`x-mas`(Coordinate(1, 2)) should be(true)
      exampleModel.`x-mas`(Coordinate(2, 2)) should be(false)
      exampleModel.`x-mas`(Coordinate(1, 3)) should be(false)
      exampleModel.`x-mas`(Coordinate(2, 3)) should be(false)

      exampleModel.`x-mas`(Coordinate(2, 6)) should be(true)
      exampleModel.`x-mas`(Coordinate(2, 7)) should be(true)
    }
    "count all XMAS's per coordinate" in {
      exampleModel.countXmas(Coordinate(4, 6)) should be (2)
    }

    "parse input" in {
      exampleModel should be(
        Grid[Char](
          // MMMSXXMASM
          (Coordinate(0, 0), 'M'),
          (Coordinate(0, 1), 'M'),
          (Coordinate(0, 2), 'M'),
          (Coordinate(0, 3), 'S'),
          (Coordinate(0, 4), 'X'),
          (Coordinate(0, 5), 'X'),
          (Coordinate(0, 6), 'M'),
          (Coordinate(0, 7), 'A'),
          (Coordinate(0, 8), 'S'),
          (Coordinate(0, 9), 'M'),

          // MSAMXMSMSA
          (Coordinate(1, 0), 'M'),
          (Coordinate(1, 1), 'S'),
          (Coordinate(1, 2), 'A'),
          (Coordinate(1, 3), 'M'),
          (Coordinate(1, 4), 'X'),
          (Coordinate(1, 5), 'M'),
          (Coordinate(1, 6), 'S'),
          (Coordinate(1, 7), 'M'),
          (Coordinate(1, 8), 'S'),
          (Coordinate(1, 9), 'A'),

          // AMXSXMAAMM
          (Coordinate(2, 0), 'A'),
          (Coordinate(2, 1), 'M'),
          (Coordinate(2, 2), 'X'),
          (Coordinate(2, 3), 'S'),
          (Coordinate(2, 4), 'X'),
          (Coordinate(2, 5), 'M'),
          (Coordinate(2, 6), 'A'),
          (Coordinate(2, 7), 'A'),
          (Coordinate(2, 8), 'M'),
          (Coordinate(2, 9), 'M'),

          // MSAMASMSMX
          (Coordinate(3, 0), 'M'),
          (Coordinate(3, 1), 'S'),
          (Coordinate(3, 2), 'A'),
          (Coordinate(3, 3), 'M'),
          (Coordinate(3, 4), 'A'),
          (Coordinate(3, 5), 'S'),
          (Coordinate(3, 6), 'M'),
          (Coordinate(3, 7), 'S'),
          (Coordinate(3, 8), 'M'),
          (Coordinate(3, 9), 'X'),

          // XMASAMXAMM
          (Coordinate(4, 0), 'X'),
          (Coordinate(4, 1), 'M'),
          (Coordinate(4, 2), 'A'),
          (Coordinate(4, 3), 'S'),
          (Coordinate(4, 4), 'A'),
          (Coordinate(4, 5), 'M'),
          (Coordinate(4, 6), 'X'),
          (Coordinate(4, 7), 'A'),
          (Coordinate(4, 8), 'M'),
          (Coordinate(4, 9), 'M'),

          // XXAMMXXAMA
          (Coordinate(5, 0), 'X'),
          (Coordinate(5, 1), 'X'),
          (Coordinate(5, 2), 'A'),
          (Coordinate(5, 3), 'M'),
          (Coordinate(5, 4), 'M'),
          (Coordinate(5, 5), 'X'),
          (Coordinate(5, 6), 'X'),
          (Coordinate(5, 7), 'A'),
          (Coordinate(5, 8), 'M'),
          (Coordinate(5, 9), 'A'),

          // SMSMSASXSS
          (Coordinate(6, 0), 'S'),
          (Coordinate(6, 1), 'M'),
          (Coordinate(6, 2), 'S'),
          (Coordinate(6, 3), 'M'),
          (Coordinate(6, 4), 'S'),
          (Coordinate(6, 5), 'A'),
          (Coordinate(6, 6), 'S'),
          (Coordinate(6, 7), 'X'),
          (Coordinate(6, 8), 'S'),
          (Coordinate(6, 9), 'S'),

          // SAXAMASAAA

          (Coordinate(7, 0), 'S'),
          (Coordinate(7, 1), 'A'),
          (Coordinate(7, 2), 'X'),
          (Coordinate(7, 3), 'A'),
          (Coordinate(7, 4), 'M'),
          (Coordinate(7, 5), 'A'),
          (Coordinate(7, 6), 'S'),
          (Coordinate(7, 7), 'A'),
          (Coordinate(7, 8), 'A'),
          (Coordinate(7, 9), 'A'),

          // MAMMMXMMMM
          (Coordinate(8, 0), 'M'),
          (Coordinate(8, 1), 'A'),
          (Coordinate(8, 2), 'M'),
          (Coordinate(8, 3), 'M'),
          (Coordinate(8, 4), 'M'),
          (Coordinate(8, 5), 'X'),
          (Coordinate(8, 6), 'M'),
          (Coordinate(8, 7), 'M'),
          (Coordinate(8, 8), 'M'),
          (Coordinate(8, 9), 'M'),

          // MXMXAXMASX
          (Coordinate(9, 0), 'M'),
          (Coordinate(9, 1), 'X'),
          (Coordinate(9, 2), 'M'),
          (Coordinate(9, 3), 'X'),
          (Coordinate(9, 4), 'A'),
          (Coordinate(9, 5), 'X'),
          (Coordinate(9, 6), 'M'),
          (Coordinate(9, 7), 'A'),
          (Coordinate(9, 8), 'S'),
          (Coordinate(9, 9), 'X'),
        )
      )
    }

    "Part1: example answer" in {
      Day4.part1(exampleModel) should be(18)
    }

    "Part2: example answer" in {
      Day4.part2(exampleModel) should be(9)
    }
  }
}