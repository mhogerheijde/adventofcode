package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Day

object Day10 extends Day[Long, Long]{
  type Model = Seq[Long]

  override def parse(input: String): Day10.Model = {
    val read = input.linesIterator.map(_.toLong).toSeq
    (0L +: read :+ (read.max + 3))
  }

  override def part1(input: Day10.Model): Long = {
    val differences = input
      .sorted
      .sliding(2)
      .map { case head +: tail => tail.foldLeft(head) { (acc, next) => next - acc }}
      .toSeq
    differences.count(_ == 1) * differences.count(_ == 3)
  }


  override def part2(input: Day10.Model): Long = ???
}
