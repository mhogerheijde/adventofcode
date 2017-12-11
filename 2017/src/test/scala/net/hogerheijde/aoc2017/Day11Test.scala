package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day11.Day11
import net.hogerheijde.aoc2017.day11.NorthEast
import net.hogerheijde.aoc2017.day11.South
import net.hogerheijde.aoc2017.day11.SouthEast
import net.hogerheijde.aoc2017.day11.SouthWest
import org.scalatest.Matchers
import org.scalatest.WordSpec


class Day11Test extends WordSpec with Matchers {

  val input1 = "ne,ne,ne"
  val input2 = "ne,ne,sw,sw"
  val input3 = "ne,ne,s,s"
  val input4 = "se,sw,se,sw,sw"

  val parse1 = Day11.parse(input1)

  "Day 11" should {
    "parse input" in {
      Day11.parse(input1) should be (IndexedSeq(NorthEast, NorthEast, NorthEast))
      Day11.parse(input2) should be (IndexedSeq(NorthEast, NorthEast, SouthWest, SouthWest))
      Day11.parse(input3) should be (IndexedSeq(NorthEast, NorthEast, South, South))
      Day11.parse(input4) should be (IndexedSeq(SouthEast, SouthWest, SouthEast, SouthWest, SouthWest))
    }


    "calculate distance" in {
      Day11.part1(Day11.parse(input1)) should be (3)
      Day11.part1(Day11.parse(input2)) should be (0)
      Day11.part1(Day11.parse(input3)) should be (2)
      Day11.part1(Day11.parse(input4)) should be (3)
    }

  }

}
