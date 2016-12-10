package net.hogerheijde.aoc2016.model

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class CoordinatesTest extends FlatSpec with Matchers {

  "Coordinates" should "update correctly" in {
    Coordinates(1, 2).update(North, 4) should be (Coordinates( 5,  2))
    Coordinates(1, 2).update(South, 4) should be (Coordinates(-3,  2))
    Coordinates(1, 2).update(East,  4) should be (Coordinates( 1,  6))
    Coordinates(1, 2).update(West,  4) should be (Coordinates( 1, -2))

    Coordinates(-1, 2).update(North, 4) should be (Coordinates( 3,  2))
    Coordinates(-1, 2).update(South, 4) should be (Coordinates(-5,  2))
    Coordinates(1, -2).update(East,  4) should be (Coordinates( 1,  2))
    Coordinates(1, -2).update(West,  4) should be (Coordinates( 1, -6))
  }

  it should "calculate distance correctly" in {
    Coordinates(2, 10).distance should be (12)
    Coordinates(-2, -2).distance should be (4)
    Coordinates(-2, 2).distance should be (4)
    Coordinates(2, -10).distance should be (12)
  }

}
