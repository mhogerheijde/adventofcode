package net.hogerheijde.aoc.common.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GridTest extends AnyWordSpec with Matchers {

  "Grid" should {
    "pretty print" in {
      Grid(
        Coordinate(0, 0) -> "a",
        Coordinate(0, 1) -> "b",
        Coordinate(0, 2) -> "c",
        Coordinate(0, 3) -> "d",
        Coordinate(0, 4) -> "e",

        Coordinate(1, 0) -> "f",
        Coordinate(1, 1) -> "g",
        Coordinate(1, 2) -> "h",
        Coordinate(1, 3) -> "i",
        Coordinate(1, 4) -> "j",

        Coordinate(2, 0) -> "k",
        Coordinate(2, 1) -> "l",
        Coordinate(2, 2) -> "m",
        Coordinate(2, 3) -> "n",
        Coordinate(2, 4) -> "o",

        Coordinate(3, 0) -> "p",
        Coordinate(3, 1) -> "q",
        Coordinate(3, 2) -> "r",
        Coordinate(3, 3) -> "s",
        Coordinate(3, 4) -> "t",

        Coordinate(4, 0) -> "u",
        Coordinate(4, 1) -> "v",
        Coordinate(4, 2) -> "w",
        Coordinate(4, 3) -> "x",
        Coordinate(4, 4) -> "y",

        Coordinate(5, 0) -> "z",
        Coordinate(5, 1) -> ".",
        Coordinate(5, 2) -> ".",
        Coordinate(5, 3) -> ".",
        Coordinate(5, 4) -> ".",
      ).pretty should be (
        """abcde
          |fghij
          |klmno
          |pqrst
          |uvwxy
          |z....""".stripMargin
      )



    }
  }
}
