package net.hogerheijde.aoc2018.day13

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day13.Model.Grid

object Day13 extends Day2018[Grid, Coordinate, Coordinate]{
  override def name: String = "Day 13"

  override def parse(input: String): Grid = Grid.parse(input)

  override def part1(input: Grid): Coordinate = input.collision

  override def part2(input: Grid): Coordinate = input.lastRemaining()
}
