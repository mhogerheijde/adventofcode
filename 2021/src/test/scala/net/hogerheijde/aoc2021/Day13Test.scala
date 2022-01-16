package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2021.Day13.Fold
import net.hogerheijde.aoc2021.Day13.Folding
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day13Test extends AnyWordSpec with Matchers {

  val exampleInput1 = Day13.parse(
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |""".stripMargin
  )

  "Parser" should {
    "parse example" in {
      exampleInput1 should be (
        Folding(
          Set(
            Coordinate(6, 10),
            Coordinate(0, 14),
            Coordinate(9, 10),
            Coordinate(0, 3),
            Coordinate(10, 4),
            Coordinate(4, 11),
            Coordinate(6, 0),
            Coordinate(6, 12),
            Coordinate(4, 1),
            Coordinate(0, 13),
            Coordinate(10, 12),
            Coordinate(3, 4),
            Coordinate(3, 0),
            Coordinate(8, 4),
            Coordinate(1, 10),
            Coordinate(2, 14),
            Coordinate(8, 10),
            Coordinate(9, 0),
          ),
          Seq(
            Fold.Horizontal(7),
            Fold.Vertical(5),
          ),
        )
      )
      println(exampleInput1)
    }
  }

  "Model" should {
    "have correct amount of dots" in {
      val afterFold = exampleInput1.fold.get
      println(afterFold)
      afterFold.dots should have (size(17))
      afterFold.fold.get.toString should be (
        """#####
          |#...#
          |#...#
          |#...#
          |#####
          |""".stripMargin
      )
    }
  }
}
