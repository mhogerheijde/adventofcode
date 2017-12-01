package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.day2.Box
import net.hogerheijde.aoc2015.day2.Day2
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestDay2 extends FlatSpec with Matchers {


  "Day 2 parser" should "parse Boxes" in {
    val expectedBoxes = IndexedSeq(
      Box(1, 2, 3),
      Box(10, 3, 5),
      Box(4, 77, 90)
    )

    Day2.parse(
      """1x2x3
        |10x3x5
        |4x77x90
        |""".stripMargin) should be (expectedBoxes)

  }

  "a Box" should "calculate area" in {
    Box(2, 3, 4).wrappingPaper should be (58)
    Box(1, 1, 10).wrappingPaper should be (43)
  }

  it should "calculate ribbon" in {
    Box(2, 3, 4).ribbon should be (34)
    Box(1, 1, 10).ribbon should be (14)
  }
}
