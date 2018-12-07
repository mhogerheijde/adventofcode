package net.hogerheijde.aoc2018.day3

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day3.Model.Fabric
import net.hogerheijde.aoc2018.day3.Model.Square

object Day3 extends Day2018[Fabric, Int, Option[Int]] {
  override def name: String = "Day 3"
  override def parse(input: String): Fabric = {
    Fabric(Parser.standardLineSplit(input).flatMap(Square.parse).toSet)
  }

  override def part1(input: Fabric): Int = {
    input.toString.count(_ == 'X')
  }
  override def part2(input: Fabric): Option[Int] = {

    val asSeq = input.squares.toSeq

    val combinations = asSeq.map ( square =>
      (square, input.squares.filterNot(_ == square))
    ).toMap

    val r = combinations.find { case (squareUnderTest, listToTestFor) =>
      listToTestFor.forall(test => squareUnderTest.doesNotIntersect(test))
    }

    r.map(_._1.id)
  }
}

