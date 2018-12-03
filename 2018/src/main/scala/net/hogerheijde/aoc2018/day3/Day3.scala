package net.hogerheijde.aoc2018.day3

import net.hogerheijde.aoc.util.Parse
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day3.Model.Fabric
import net.hogerheijde.aoc2018.day3.Model.Square

object Day3 extends Day2018[Fabric, Int, Option[Int]] {
  override def name: String = "Day 3"
  override def parse(input: String): Fabric = {
    Fabric(Parse.standardLineSplit(input).flatMap(Square.parse).toSet)
  }

  override def part1(input: Fabric): Int = {
    input.toString.count(_ == 'X')
  }
  override def part2(input: Fabric): Option[Int] = {
    val combinations = input.squares.toSeq.combinations(2).toSeq

    combinations.find(pairs =>
      pairs(0).doesNotIntersect(pairs(1))
    ).map(_(1).id) // Cheating, it wasn't the left one, so it must be the right one.
  }
}

