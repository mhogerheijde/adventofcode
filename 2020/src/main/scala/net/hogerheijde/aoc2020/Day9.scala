package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Day

object Day9 extends Day[Long, Long] {
  type Model = Seq[Long]

  override def parse(input: String): Model = input.linesIterator.map(_.toLong).toSeq

  override def part1(input: Model): Long = {
    findOffender(25, input)
  }


  override def part2(input: Model): Long = {
    val target = part1(input)
    (2 to input.size)
      .map { windowSize =>
        input.sliding(windowSize)
          .find { window => window.sum == target }
          .map { window => window.min + window.max}
      }
      .collectFirst { case Some(result) => result }
      .get
  }

  def findOffender(preambleSize: Int, sequence: Model): Long = {
    sequence
      .sliding(preambleSize + 1)
      .find { case preamble :+ last => !preamble.combinations(2).map(_.sum).contains(last) }
      .map(_.last)
      .get
  }
}
