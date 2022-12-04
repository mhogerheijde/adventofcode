package net.hogerheijde.aoc2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test extends AnyWordSpec with Matchers {

  val exampleInput =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      |""".stripMargin

  val exampleModel = Seq(
    (r(2, 4) , r(6, 8)),
    (r(2, 3) , r(4, 5)),
    (r(5, 7) , r(7, 9)),
    (r(2, 8) , r(3, 7)),
    (r(6, 6) , r(4, 6)),
    (r(2, 6) , r(4, 8)),
  )

  def r(start: Int, end: Int): Range = Range.inclusive(start, end)

  "Day 4" should {
    "parse" in {
      Day4.parse(exampleInput) should be(exampleModel)
    }

    "solve part 1" in {
      Day4.part1(exampleModel) should be(2)
    }
    "solve part 2" in {
      Day4.part2(exampleModel) should be(4)
    }
  }

}
