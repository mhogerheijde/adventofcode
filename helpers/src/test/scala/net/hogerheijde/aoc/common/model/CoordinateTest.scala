package net.hogerheijde.aoc.common.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CoordinateTest extends AnyWordSpec with Matchers {
  "Coordinate distance" should {
    "calculate distance horizontally" in {
      Range(-25, 25).foreach { y =>
        Coordinate(1, y).distance(Coordinate(10, y)) should be(9)
        Coordinate(-10, y).distance(Coordinate(10, y)) should be(20)
        Coordinate(1, y).distance(Coordinate(-1, y)) should be(2)
        Coordinate(9, y).distance(Coordinate(10, y)) should be(1)
      }
    }

    "calculate distance vertically" in {
      Range(-25, 25).foreach { x =>
        Coordinate(x, 1).distance(Coordinate(x, 10)) should be(9)
        Coordinate(x, -10).distance(Coordinate(x, 10)) should be(20)
        Coordinate(x, 1).distance(Coordinate(x, -1)) should be(2)
        Coordinate(x, 9).distance(Coordinate(x, 10)) should be(1)
      }
    }

    "calculate distance on a slope" in {
      // √2
      Coordinate(1, 1).distance(Coordinate(2,2)) should be (1.4142135623730951)
      Coordinate(0, 0).distance(Coordinate(3,4)) should be (5)
    }

  }

  "Coordinate range" should {
    "expand" in {
      Coordinate.range(Coordinate(0, 0), Coordinate(4, 5)) should be (Seq(
        Coordinate(0, 0),
        Coordinate(0, 1),
        Coordinate(0, 2),
        Coordinate(0, 3),
        Coordinate(0, 4),
        Coordinate(1, 0),
        Coordinate(1, 1),
        Coordinate(1, 2),
        Coordinate(1, 3),
        Coordinate(1, 4),
        Coordinate(2, 0),
        Coordinate(2, 1),
        Coordinate(2, 2),
        Coordinate(2, 3),
        Coordinate(2, 4),
        Coordinate(3, 0),
        Coordinate(3, 1),
        Coordinate(3, 2),
        Coordinate(3, 3),
        Coordinate(3, 4),
      ))



    }
  }


}
