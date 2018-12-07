package net.hogerheijde.aoc2018.day6

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.parser.Common.coordinate
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.util.Timer
import net.hogerheijde.aoc.util.Timer.TimedResult
import net.hogerheijde.aoc2018.Day2018


object Day6 extends Day2018[Grid, Int, Int]{
  override def name: String = "Day 6"

  private val parseCoordinate: String => Option[Coordinate] = Parser.parse(coordinate(_))

  // Char 930 has issues :'(
  private val letters = (
    Range('A', 'Z').toSeq ++
      Range('Б', 'Я').toSeq ++
      Range('Α', 'Ω').toSeq
  ).map(_.toChar).filter(_.isUpper)

  override def parse(input: String): Grid = {
    val coordinates = input.trim.lines.flatMap(input => parseCoordinate(input.trim)).toIndexedSeq

    Grid((coordinates.zip(letters)).map {
      case (coord, id) => NamedCoordinate(id, coord)
    })
  }

  override def part1(input: Grid): Int = {
    val TimedResult(newGrid, d1) = Timer(input.withAreasResolved)
    println(s"Calculating new grid: $d1")
    val TimedResult(area, d2) = Timer(newGrid.maxArea)
    println(s"Finding max area: $d2")
    area
  }

  override def part2(input: Grid): Int = {
    val TimedResult(newGrid, d1) = Timer(input.toRegionGrid(10000))
    println(s"Calculating new grid: $d1")
    val TimedResult(region, d2) = Timer(newGrid.regionSize)
    println(s"Finding region size: $d2")
    region
  }
}
