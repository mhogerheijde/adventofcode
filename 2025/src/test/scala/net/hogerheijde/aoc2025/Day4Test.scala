package net.hogerheijde.aoc2025

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc2025.Day4.Tile
import net.hogerheijde.aoc2025.Day4.Tile.Empty
import net.hogerheijde.aoc2025.Day4.Tile.PaperRoll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.
      |""".stripMargin


  val exampleModel = Grid(
    Seq(
      Seq(Empty, Empty, PaperRoll, PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty)
        .zipWithIndex.map((tile,  x) => Coordinate(0, x) -> tile),
      Seq(PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll, Empty, PaperRoll, Empty, PaperRoll, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(1, x) -> tile),
      Seq(PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll, Empty, PaperRoll, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(2, x) -> tile),
      Seq(PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty, Empty, PaperRoll, Empty)
        .zipWithIndex.map((tile,  x) => Coordinate(3, x) -> tile),
      Seq(PaperRoll, PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(4, x) -> tile),
      Seq(Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(5, x) -> tile),
      Seq(Empty, PaperRoll, Empty, PaperRoll, Empty, PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(6, x) -> tile),
      Seq(PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll)
        .zipWithIndex.map((tile,  x) => Coordinate(7, x) -> tile),
      Seq(Empty, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, PaperRoll, Empty)
        .zipWithIndex.map((tile,  x) => Coordinate(8, x) -> tile),
      Seq(PaperRoll, Empty, PaperRoll, Empty, PaperRoll, PaperRoll, PaperRoll, Empty, PaperRoll, Empty)
        .zipWithIndex.map((tile,  x) => Coordinate(9, x) -> tile),
    ).flatten


  )

  "Grid" should {
    "PaperRoll is accessible" in {
      exampleModel.isAccessiblePaperRoll(Coordinate(0,2)) should be(true) // Fewer than 4 adjacent rolls
      exampleModel.isAccessiblePaperRoll(Coordinate(0,0)) should be(false) // not a paper roll
      exampleModel.isAccessiblePaperRoll(Coordinate(5,5)) should be(false) // More than or equal to 4 adjecent rolls
    }
  }

  "Day 4" should {

    "do a part=2 step " in {
      val step1 = Day4.parse(
        """.......@..
          |.@@.@.@.@@
          |@@@@@...@@
          |@.@@@@..@.
          |.@.@@@@.@.
          |.@@@@@@@.@
          |.@.@.@.@@@
          |..@@@.@@@@
          |.@@@@@@@@.
          |....@@@...
          |""".stripMargin
      )


      val result: (Grid[Tile], Int) = Day4.part2step(exampleModel)
      result._1 should be (step1)
      result._2 should be (13)
    }

    "parse input" in {
      Day4.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day4.part1(Day4.parse(exampleInput)) should be(13)
    }

    "Part2: example answer" in {
      Day4.part2(Day4.parse(exampleInput)) should be(43)
    }
  }
}