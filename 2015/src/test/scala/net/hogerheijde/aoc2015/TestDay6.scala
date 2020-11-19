package net.hogerheijde.aoc2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import net.hogerheijde.aoc2015.day6.Range
import net.hogerheijde.aoc2015.day6.Coordinate
import net.hogerheijde.aoc2015.day6.Day6
import net.hogerheijde.aoc2015.day6.Grid
import net.hogerheijde.aoc2015.day6.Instruction
import net.hogerheijde.aoc2015.day6.State

class TestDay6 extends AnyFlatSpec with Matchers {

  "Range" should "create iterator" in {
    Range.unapply("0,0 through 0,0") should be (Some(Range(Coordinate(0, 0), Coordinate(0, 0))))

    Range(Coordinate(0, 0), Coordinate(0, 0)).iterator.toSeq should be (
      Seq(Coordinate(0, 0))
    )

    Range(Coordinate(1, 3), Coordinate(5, 5)).iterator.toSeq should be (
      Seq(
        Coordinate(1, 3),
        Coordinate(2, 3),
        Coordinate(3, 3),
        Coordinate(4, 3),
        Coordinate(5, 3),
        Coordinate(1, 4),
        Coordinate(2, 4),
        Coordinate(3, 4),
        Coordinate(4, 4),
        Coordinate(5, 4),
        Coordinate(1, 5),
        Coordinate(2, 5),
        Coordinate(3, 5),
        Coordinate(4, 5),
        Coordinate(5, 5),
      )
    )
  }

  "Grid" should "count" in {
    Grid(Set(
      Coordinate(1, 1),
      Coordinate(2, 1),
      Coordinate(3, 1),
      Coordinate(4, 1),
      Coordinate(8, 1),
      Coordinate(1, 30),
    )).count() should be (6)
  }

  "Day6 - part 2" should "work" in {
    Day6.part2(Seq(Instruction.unapply("turn on 0,0 through 0,0")).flatten) should be (1)
    Day6.part2(Seq(Instruction.unapply("toggle 0,0 through 999,999")).flatten) should be (2_000_000)
  }

}
