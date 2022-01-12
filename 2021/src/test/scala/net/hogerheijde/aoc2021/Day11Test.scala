
package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.DigitGrid
import net.hogerheijde.aoc2021.Day11.DigitGridHelper
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day11Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526
        |""".stripMargin
    Day11.parse(input)
  }

  "Model" should {
    "should take step" in {
      val input =
        """12
          |34
          |""".stripMargin
      val model = Day11.parse(input)

      model.step._1 should be (DigitGrid(Map(
        Coordinate(0, 0) -> 2,
        Coordinate(1, 0) -> 3,
        Coordinate(0, 1) -> 4,
        Coordinate(1, 1) -> 5,
      )))
    }
    "should flash" in {
      val input =
        """12
          |39
          |""".stripMargin
      val model = Day11.parse(input)

      model.step._1 should be(DigitGrid(Map(
        Coordinate(0, 0) -> 3,
        Coordinate(1, 0) -> 4,
        Coordinate(0, 1) -> 5,
        Coordinate(1, 1) -> 0,
      )))
    }

    "step through tiny example" in {
      val input =
        """11111
          |19991
          |19191
          |19991
          |11111
          |""".stripMargin
      val modelInput = Day11.parse(input)

      val expect =
        """34543
          |40004
          |50005
          |40004
          |34543
          |""".stripMargin
      val modelExpectStep1 = Day11.parse(expect)

      val expect2 =
        """45654
          |51115
          |61116
          |51115
          |45654
          |""".stripMargin
      val modelExpectStep2 = Day11.parse(expect2)

      val (step1, count1) = modelInput.step
      val (step2, count2) = step1.step

      step1 should be (modelExpectStep1)
      count1 should be (9)
      step2 should be (modelExpectStep2)
      count2 should be (0)
    }


    "do 1 steps on the larger example" in {
      val (newGrid, count) = exampleInput.step
      count should be (0)
      newGrid should be (Day11.parse(
        """6594254334
          |3856965822
          |6375667284
          |7252447257
          |7468496589
          |5278635756
          |3287952832
          |7993992245
          |5957959665
          |6394862637
          |""".stripMargin))
    }

    "do 2 steps on the larger example" in {
      val (newGrid, count) = exampleInput.step(2)
      count should be (35)
      newGrid should be (Day11.parse(
        """8807476555
          |5089087054
          |8597889608
          |8485769600
          |8700908800
          |6600088989
          |6800005943
          |0000007456
          |9000000876
          |8700006848
          |""".stripMargin))
    }

    "do 10 steps on the larger example" in {
      val (newGrid, count) = exampleInput.step(10)
//      count should be (204)
      newGrid should be (Day11.parse(
        """0481112976
          |0031112009
          |0041112504
          |0081111406
          |0099111306
          |0093511233
          |0442361130
          |5532252350
          |0532250600
          |0032240000
          |""".stripMargin))
    }
    "do 100 steps on the larger example" in {
      val (newGrid, count) = exampleInput.step(100)
      count should be (1656)
      newGrid should be(Day11.parse(
        """0397666866
          |0749766918
          |0053976933
          |0004297822
          |0004229892
          |0053222877
          |0532222966
          |9322228966
          |7922286866
          |6789998766
          |""".stripMargin))
    }
  }

  "Day 11" should {
    "part 1" in { Day11.part1(exampleInput) should be (1656) }
    "part 2" in { Day11.part2(exampleInput) should be (195) }
  }
}
