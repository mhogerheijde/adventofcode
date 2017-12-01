package net.hogerheijde.aoc2015.day3

import net.hogerheijde.aoc2015.util.Day

import scala.collection.immutable.IndexedSeq

object Day3 extends Day[IndexedSeq[Direction], Int, Int] {
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 3"
  override def parse: String => IndexedSeq[Direction] = _.map(Direction(_))

  override def part1(input: IndexedSeq[Direction]): Int = {
    val housesVisited = input.foldLeft(IndexedSeq(Coordinate(0, 0))) { (visited, direction) =>
        nextCoordinate(visited, direction)
    }
    housesVisited.distinct.length
  }

  override def part2(input: IndexedSeq[Direction]): Int = {
    val (stantaVisits, robotVisits) = input.sliding(2, 2).foldLeft( (IndexedSeq(Coordinate(0, 0)), IndexedSeq(Coordinate(0, 0))) ) {
      case ((santa, robot), directions) =>

        ( nextCoordinate(santa, directions(0)), nextCoordinate(robot, directions(1)) )
    }
    (stantaVisits ++ robotVisits).distinct.length
  }


  private def nextCoordinate(visited: IndexedSeq[Coordinate], direction: Direction) = visited :+ visited.last.move(direction)
}
