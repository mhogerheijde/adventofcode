package net.hogerheijde.aoc2018.day18

import net.hogerheijde.aoc.common.model.Coordinate
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day18Test extends WordSpec with Matchers {

  "Day18" should {

    "parse" in {
      val example =
        """.#.#...|#.
          |.....#|##|
          |.|..|...#.
          |..|#.....#
          |#.#|||#|#|
          |...#.||...
          |.|....|...
          |||...#|.#|
          ||.||||..|.
          |...#.|..|.
        """.stripMargin

      val result = Day18.parse(example)

      result.maxX should be (9)
      result.maxY should be (9)
      result.acres(Coordinate(0,0)) should be(Open)
      result.acres(Coordinate(8,0)) should be(Lumberyard)
      result.acres(Coordinate(9,1)) should be(Tree)
    }
  }

  "Grid" should {

    val exampleGrid = Day18.parse(
        """.#.#...|#.
          |.....#|##|
          |.|..|...#.
          |..|#.....#
          |#.#|||#|#|
          |...#.||...
          |.|....|...
          |||...#|.#|
          ||.||||..|.
          |...#.|..|.
        """.stripMargin)



    "calculate next" in {

      exampleGrid.next.toString should be (
        """.......##.
          |......|###
          |.|..|...#.
          |..|#||...#
          |..##||.|#|
          |...#||||..
          |||...|||..
          ||||||.||.|
          |||||||||||
          |....||..|.""".stripMargin)

      exampleGrid.next(10).toString should be (
        """.||##.....
          |||###.....
          |||##......
          ||##.....##
          ||##.....##
          ||##....##|
          |||##.####|
          |||#####|||
          |||||#|||||
          |||||||||||""".stripMargin)

    }

    "calculate score" in {

      val grid = Day18.parse(
        """.||##.....
          |||###.....
          |||##......
          ||##.....##
          ||##.....##
          ||##....##|
          |||##.####|
          |||#####|||
          |||||#|||||
          |||||||||||""".stripMargin)

      grid.score should be (1147)



    }

  }

}
