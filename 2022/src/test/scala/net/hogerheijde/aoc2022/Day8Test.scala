package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2022.Day8.Trees
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day8Test extends AnyWordSpec with Matchers {


  val exampleInput =
    """30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin

  val exampleGrid = Trees(
    rows = IndexedSeq(
      IndexedSeq(3, 0, 3, 7, 3),
      IndexedSeq(2, 5, 5, 1, 2),
      IndexedSeq(6, 5, 3, 3, 2),
      IndexedSeq(3, 3, 5, 4, 9),
      IndexedSeq(3, 5, 3, 9, 0),
    ),
    columns = IndexedSeq(
      IndexedSeq(3, 2, 6, 3, 3),
      IndexedSeq(0, 5, 5, 3, 5),
      IndexedSeq(3, 5, 3, 5, 3),
      IndexedSeq(7, 1, 3, 4, 9),
      IndexedSeq(3, 2, 2, 9, 0),
    )
  )


  "Day 8" should {
    "parse example" in {
      Day8.parse(exampleInput) should be(exampleGrid)
    }

    "solve part 1" in {
      Day8.part1(exampleGrid) should be(21)
    }
    "solve part 2" in {
      Day8.part2(exampleGrid) should be(8)
    }
  }

  "Trees" should {
    "give visibility for all trees on the edge" in {
      val rim = Seq(
        (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), // top row
        (1, 0), (2, 0), (3, 0), // left
        (1, 4), (2, 4), (3, 4), // right
        (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), // bottom row
      ).map(Coordinate.apply)

      rim.foreach { tree =>
        val x = exampleGrid.isVisible(tree)
        x should be(true)
      }
    }

    "give visibility for other trees" in {
      exampleGrid.isVisible(Coordinate(1, 1)) should be(true)
      exampleGrid.isVisible(Coordinate(1, 2)) should be(true)
      exampleGrid.isVisible(Coordinate(1, 3)) should be(false)

      exampleGrid.isVisible(Coordinate(2, 1)) should be(true)
      exampleGrid.isVisible(Coordinate(2, 2)) should be(false)
      exampleGrid.isVisible(Coordinate(2, 3)) should be(true)

      exampleGrid.isVisible(Coordinate(3, 1)) should be(false)
      exampleGrid.isVisible(Coordinate(3, 2)) should be(true)
      exampleGrid.isVisible(Coordinate(3, 3)) should be(false)

    }

    "list all coordinates" in {
      Coordinate.range(Coordinate(0, 0), Coordinate(exampleGrid.rows.size, exampleGrid.columns.size)) should be (
        Seq(
          (0, 0), (0, 1), (0, 2), (0, 3), (0, 4),
          (1, 0), (1, 1), (1, 2), (1, 3), (1, 4),
          (2, 0), (2, 1), (2, 2), (2, 3), (2, 4),
          (3, 0), (3, 1), (3, 2), (3, 3), (3, 4),
          (4, 0), (4, 1), (4, 2), (4, 3), (4, 4),
        ).map(Coordinate.apply)
      )
    }

  }

}
