package net.hogerheijde.aoc2016.days.day2

import net.hogerheijde.aoc2016.days.day2.model.Eight
import net.hogerheijde.aoc2016.days.day2.model.Five
import net.hogerheijde.aoc2016.days.day2.model.GoDown
import net.hogerheijde.aoc2016.days.day2.model.GoLeft
import net.hogerheijde.aoc2016.days.day2.model.GoRight
import net.hogerheijde.aoc2016.days.day2.model.GoUp
import net.hogerheijde.aoc2016.days.day2.model.Nine
import net.hogerheijde.aoc2016.days.day2.model.One
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.immutable.IndexedSeq

class Day2Test extends FlatSpec with Matchers {




  "Day2" should "calculate the correct code for the example" in {

    // SETUP
    val input = "ULL\nRRDDD\nLURDL\nUUUUD"
    val expected = "1985"

    // CALL
    val result = Day2.process(input)

    result should be(expected)

  }

  it should "parse a list of instructions correctly" in {
    // SETUP
    val input = "ULL\nRRDDD\nLURDL\nUUUUD"
    val expected = IndexedSeq(
      IndexedSeq(GoUp, GoLeft, GoLeft),
      IndexedSeq(GoRight, GoRight, GoDown, GoDown, GoDown),
      IndexedSeq(GoLeft, GoUp, GoRight, GoDown, GoLeft),
      IndexedSeq(GoUp, GoUp, GoUp, GoUp, GoDown)
    )

    // CALL
    val result = Day2.parseInstructions(input)

    // VERIFY
    result should be(expected)

  }

  it should "infer the correct code from the instructions" in {
    // SETUP
    val instructions = IndexedSeq(
      IndexedSeq(GoUp, GoLeft, GoLeft),
      IndexedSeq(GoRight, GoRight, GoDown, GoDown, GoDown),
      IndexedSeq(GoLeft, GoUp, GoRight, GoDown, GoLeft),
      IndexedSeq(GoUp, GoUp, GoUp, GoUp, GoDown)
    )
    val expect = IndexedSeq(One, Nine, Eight, Five)

    // CALL
    val result = Day2.processInstructions(instructions)

    // VERIFY
    result should be(expect)

  }

}
