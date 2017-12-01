package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.day3.Day3
import net.hogerheijde.aoc2015.day3.East
import net.hogerheijde.aoc2015.day3.North
import net.hogerheijde.aoc2015.day3.South
import net.hogerheijde.aoc2015.day3.West
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.immutable.IndexedSeq

class TestDay3 extends FlatSpec with Matchers{

  "Day 3; part 1" should "calculate" in {
    Day3.part1(IndexedSeq(East)) should be(2)
    Day3.part1(IndexedSeq(North, East, South, West)) should be(4)
    Day3.part1(IndexedSeq(North, South, North, South, North, South, North, South, North, South)) should be(2)
  }

  "Day 3; part 2" should "calculate" in {
    Day3.part2(IndexedSeq(North, South)) should be (3)
    Day3.part2(IndexedSeq(North, East, South, West)) should be (3)
    Day3.part2(IndexedSeq(North, South, North, South, North, South, North, South, North, South))  should be (11)
  }



}
