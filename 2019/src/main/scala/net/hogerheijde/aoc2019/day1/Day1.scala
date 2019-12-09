package net.hogerheijde.aoc2019.day1

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2019.Day2019


object Day1 extends Day2019[Model, Int, Int] {

  override def name: String = "Day 1"

  override def parse(in: String): Model = {
    Parser.standardLineSplit(in).map(_.toInt)
  }

  def fuelForModule(moduleMass: Int): Int = {
    (Math.floor(moduleMass / 3.0) - 2).toInt
  }

  override def part1(input: Model): Int = {
    input.foldLeft(0) { case (totalFuel, nextModuleMass) =>
        fuelForModule(nextModuleMass) + totalFuel
    }
  }
  override def part2(input: Model): Int = {
    input.foldLeft(0) { case (totalFuel, nextModuleMass) =>
      val initFuel = fuelForModule(nextModuleMass)
      val thisModulerReq = rocket(initFuel, 0)

      totalFuel + thisModulerReq
    }
  }


  def rocket(mass: Int, total: Int): Int = {
    fuelForModule(mass) match {
      case fuel if fuel < 0 => total + mass
      case fuel => rocket(fuel, total + mass)
    }
  }
}
