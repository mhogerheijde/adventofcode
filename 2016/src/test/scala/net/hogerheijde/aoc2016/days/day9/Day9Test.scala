package net.hogerheijde.aoc2016.days.day9

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day9Test extends FlatSpec with Matchers {

  "Day9" should "expand correctly for examples" in {

    Day9.processPt1("ADVENT") should be(6)
    Day9.processPt1("A(1x5)BC") should be(7)
    Day9.processPt1("(3x3)XYZ") should be(9)
    Day9.processPt1("A(2x2)BCD(2x2)EFG") should be(11)
    Day9.processPt1("(6x1)(1x3)A") should be(6)
    Day9.processPt1("X(8x2)(3x3)ABCY") should be(18)

  }

  it should "calculate length correctly" in {

    Day9.lengthOf("ADVENT".iterator) should be(6)
    Day9.lengthOf("A(1x5)BC".iterator) should be(7)
    Day9.lengthOf("(3x3)XYZ".iterator) should be(9)
    Day9.lengthOf("A(2x2)BCD(2x2)EFG".iterator) should be(11)
    Day9.lengthOf("(6x1)(1x3)A".iterator) should be(6)
    Day9.lengthOf("X(8x2)(3x3)ABCY".iterator) should be(18)

  }

  it should "calculate length correctly for version 2" in {

    Day9.processPt2("(3x3)XYZ") should be(9)
    Day9.processPt2("X(8x2)(3x3)ABCY") should be(20)

    Day9.processPt2("(1x12)A") should be(12)
    Day9.processPt2("(7x10)(1x12)A") should be (120)
    Day9.processPt2("(13x14)(7x10)(1x12)A") should be(1680)
    Day9.processPt2("(20x12)(13x14)(7x10)(1x12)A") should be(20160)
    Day9.processPt2("(27x12)(20x12)(13x14)(7x10)(1x12)A") should be(241920)

    Day9.processPt2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") should be(445)
  }
}