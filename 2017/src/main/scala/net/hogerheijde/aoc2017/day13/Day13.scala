package net.hogerheijde.aoc2017.day13

import net.hogerheijde.aoc2017.Day2017

object Day13 extends Day2017[Firewall, Int, Unit]{
  override def name: String = "Day 13"

  override def parse(input: String): Firewall = {
    val layers = input.trim.lines.map { layer =>
      val layerparts = layer.split(":")
      (layerparts(0).trim.toInt, ActiveLayer(layerparts(1).trim.toInt))
    }.toMap

    Firewall(layers)
  }
  override def part1(input: Firewall): Int = {
    input.severity
  }
  override def part2(input: Firewall): Unit = ???
}
