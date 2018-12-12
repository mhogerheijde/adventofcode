package net.hogerheijde.aoc2018.day11

import net.hogerheijde.aoc.common.model.Coordinate
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day11Test extends WordSpec with Matchers {

  "Grid" should {
    "build from Seed" in {

      Grid.build(18, 300).subgrid(Coordinate(32,44),5) should be(
        Grid(Map(
          Coordinate(31+1,44) -> -2,
          Coordinate(31+2,44) -> -4,
          Coordinate(31+3,44) -> 4,
          Coordinate(31+4,44) -> 4,
          Coordinate(31+5,44) -> 4,

          Coordinate(31+1,45) -> -4,
          Coordinate(31+2,45) -> 4,
          Coordinate(31+3,45) -> 4,
          Coordinate(31+4,45) -> 4,
          Coordinate(31+5,45) -> -5,

          Coordinate(31+1,46) -> 4,
          Coordinate(31+2,46) -> 3,
          Coordinate(31+3,46) -> 3,
          Coordinate(31+4,46) -> 4,
          Coordinate(31+5,46) -> -4,

          Coordinate(31+1,47) -> 1,
          Coordinate(31+2,47) -> 1,
          Coordinate(31+3,47) -> 2,
          Coordinate(31+4,47) -> 4,
          Coordinate(31+5,47) -> -3,

          Coordinate(31+1,48) -> -1,
          Coordinate(31+2,48) -> 0,
          Coordinate(31+3,48) -> 2,
          Coordinate(31+4,48) -> -5,
          Coordinate(31+5,48) -> -2,
        ))
      )
    }
  }

}
