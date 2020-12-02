package net.hogerheijde.aoc2019

import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day1 extends Day[Int, Int] {
  type Model = IndexedSeq[Int]

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
