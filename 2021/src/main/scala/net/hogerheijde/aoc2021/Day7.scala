package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day7 extends Day[Int, Long] {

  override type Model = Seq[Int]

  override def parse(input: String): Model = Parser.parse(intSeq(_))(input).get

  def leastFuelConsumption(input: Seq[Int])(fuelConsumptionCalculation: (Int, Int) => Int) = {
    val minPos = input.min
    val maxPos = input.max
    Range.inclusive(minPos, maxPos).foldLeft(Map.empty[Int, Int]) { case (state, position) =>
      val consumption = input.map(crabPosition => fuelConsumptionCalculation(crabPosition, position)).sum
      state.updated(position, consumption)
    }.values.min
  }

  override def part1(input: Model): Int = {
    leastFuelConsumption(input) { (crabPosition, position) =>
      Math.abs(crabPosition - position)
    }
  }

  override def part2(input: Model): Long = {
    leastFuelConsumption(input) { (crabPosition, position) =>
      val distanceTravelled = Math.abs(crabPosition - position)
      // Triangular numbers: a(n) = binomial(n+1,2) = n*(n+1)/2
      // See https://oeis.org/search?q=Triangular+numbers&sort=&language=english&go=Search
      distanceTravelled * (distanceTravelled + 1) / 2
    }

  }
}
