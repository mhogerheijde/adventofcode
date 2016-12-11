package net.hogerheijde.aoc2016.model

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class CoordinatesTest extends FlatSpec with Matchers {

  "Coordinates" should "update correctly" in {
    Coordinate(1, 2).update(North, 4) should be (Coordinate( 5,  2))
    Coordinate(1, 2).update(South, 4) should be (Coordinate(-3,  2))
    Coordinate(1, 2).update(East,  4) should be (Coordinate( 1,  6))
    Coordinate(1, 2).update(West,  4) should be (Coordinate( 1, -2))

    Coordinate(-1, 2).update(North, 4) should be (Coordinate( 3,  2))
    Coordinate(-1, 2).update(South, 4) should be (Coordinate(-5,  2))
    Coordinate(1, -2).update(East,  4) should be (Coordinate( 1,  2))
    Coordinate(1, -2).update(West,  4) should be (Coordinate( 1, -6))
  }

  it should "calculate distance correctly" in {
    Coordinate(2, 10).distance should be (12)
    Coordinate(-2, -2).distance should be (4)
    Coordinate(-2, 2).distance should be (4)
    Coordinate(2, -10).distance should be (12)
  }


  it should "expand east correctly" in {
    val expected1 = IndexedSeq(Coordinate(0, 1), Coordinate(0, 2), Coordinate(0, 3))
    Coordinate(0, 0).expand(Coordinate(0, 3)) should be(expected1)
  }

  it should "expand north correctly" in {
    val expected2 = IndexedSeq(Coordinate(1,  0), Coordinate(2,  0), Coordinate(3,  0))
    Coordinate(0, 0).expand(Coordinate(3, 0)) should be (expected2)
  }

  it should "expand west correctly" in {
    val expected3 = IndexedSeq(Coordinate(0, -1), Coordinate(0,  -2), Coordinate(0,  -3))
    Coordinate(0, 0).expand(Coordinate(0, -3)) should be (expected3)
  }

  it should "expand south correctly" in {
    val expected4 = IndexedSeq(Coordinate(-1, 0), Coordinate(-2, 0), Coordinate(-3, 0))
    Coordinate(0, 0).expand(Coordinate(-3, 0)) should be (expected4)
  }

}
