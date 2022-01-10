
package net.hogerheijde.aoc2021

import net.hogerheijde.aoc2021.Day9.Coordinate
import net.hogerheijde.aoc2021.Day9.Grid
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678
        |""".stripMargin
    Day9.parse(input)
  }




  "Parser" should {
    "parse stuff" in {
      Day9.parse(
        """21999
          |43210
          |39878
          |""".stripMargin) should be (
        Grid(
          Map(
            Coordinate(0, 0) -> 2,
            Coordinate(0, 1) -> 1,
            Coordinate(0, 2) -> 9,
            Coordinate(0, 3) -> 9,
            Coordinate(0, 4) -> 9,
            Coordinate(1, 0) -> 4,
            Coordinate(1, 1) -> 3,
            Coordinate(1, 2) -> 2,
            Coordinate(1, 3) -> 1,
            Coordinate(1, 4) -> 0,
            Coordinate(2, 0) -> 3,
            Coordinate(2, 1) -> 9,
            Coordinate(2, 2) -> 8,
            Coordinate(2, 3) -> 7,
            Coordinate(2, 4) -> 8,
          )
        )
      )
    }


  }

  "Day 9" should {
    "part 1" in { Day9.part1(exampleInput) should be (15) }
    "part 2" in { Day9.part2(exampleInput) should be (1134) }
  }
}
