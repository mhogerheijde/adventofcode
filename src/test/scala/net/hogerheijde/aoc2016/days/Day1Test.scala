package net.hogerheijde.aoc2016.days

import net.hogerheijde.aoc2016.model.Coordinates
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import net.hogerheijde.aoc2016.model.GoLeft
import net.hogerheijde.aoc2016.model.GoRight

import scala.collection.immutable.IndexedSeq

class Day1Test extends FlatSpec with Matchers {

  "Day1" should "parse a string into a direction" in {

    Day1.parseDirection("L1 ") should be (Some(GoLeft(1)))
    Day1.parseDirection(" R1") should be (Some(GoRight(1)))

    Day1.parseDirection("L5") should be (Some(GoLeft(5)))
    Day1.parseDirection("R7") should be (Some(GoRight(7)))

    Day1.parseDirection("L1537") should be (Some(GoLeft(1537)))
    Day1.parseDirection("R78") should be (Some(GoRight(78)))

  }

  it should "parse a list of directions" in  {
    // SETUP
    val directions = "L4, L1, R4, R1, R1, L3, R5"
    val expected = IndexedSeq(
      GoLeft(4),
      GoLeft(1),
      GoRight(4),
      GoRight(1),
      GoRight(1),
      GoLeft(3),
      GoRight(5)
    )

    // CALL
    val result = Day1.parse(directions)

    // VERIFY
    result should be(expected)

  }


  it should "conclude the correct end coordinates for a set of instructions" in {

    // SETUP
    val instructions1 = IndexedSeq(GoRight(2), GoLeft(3))
    val expectedCoordinates1 = Coordinates(3, 2)

    val instructions2 = IndexedSeq(GoRight(2), GoRight(2), GoRight(2))
    val expectedCoordinates2 = Coordinates(-2, 0)

    val instructions3 = IndexedSeq(GoRight(5), GoLeft(5), GoRight(5), GoRight(3))
    val expectedCoordinates3 = Coordinates(2, 10)
//      R5, L5, R5, R3

    // CALL
    val result1 = new Day1(instructions1).processInstructions()
    val result2 = new Day1(instructions2).processInstructions()
    val result3 = new Day1(instructions3).processInstructions()


    // VERIFY
    result1 should be (expectedCoordinates1)
    result2 should be (expectedCoordinates2)
    result3 should be (expectedCoordinates3)
  }

}
