package net.hogerheijde.aoc2017.day11

import net.hogerheijde.aoc2017.Day

import scala.math.max

object Day11 extends Day[IndexedSeq[Direction], Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 11"
  override def parse: String => IndexedSeq[Direction] = input => input.split(",").map(Direction.fromString).toIndexedSeq
  override def part1(input: IndexedSeq[Direction]): Int = {
    val end = input.foldLeft(Coordinate.Center) { (currentPosition, nextDirection) => currentPosition.go(nextDirection) }
    end.distance(Coordinate.Center)
  }

  override def part2(input: IndexedSeq[Direction]): Int = {
    val end = input.foldLeft((Coordinate.Center, 0)) { case ((currentPosition, maxDist), nextDirection) =>
      val nextPosition = currentPosition.go(nextDirection)
      val distance = nextPosition.distance(Coordinate.Center)
      (nextPosition, max(distance, maxDist))
    }
    end._2
  }
}
