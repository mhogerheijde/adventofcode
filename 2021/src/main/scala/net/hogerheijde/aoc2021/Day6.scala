package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day6 extends Day[Long, Long] {

  override type Model = Seq[Int]

  override def parse(input: String): Model = Parser.parse(intSeq(_))(input).get

  protected[aoc2021] def calculateDays(input: Seq[Int], noOfDays: Int): Long = {
    val histogram = Map(
      0 -> input.count(_ == 0).toLong,
      1 -> input.count(_ == 1).toLong,
      2 -> input.count(_ == 2).toLong,
      3 -> input.count(_ == 3).toLong,
      4 -> input.count(_ == 4).toLong,
      5 -> input.count(_ == 5).toLong,
      6 -> input.count(_ == 6).toLong,
      7 -> input.count(_ == 7).toLong,
      8 -> input.count(_ == 8).toLong,
    )

    Range.inclusive(1, noOfDays).foldLeft(histogram) { case (state, _) =>
      Map(
        0 -> state.getOrElse(1, 0),
        1 -> state.getOrElse(2, 0),
        2 -> state.getOrElse(3, 0),
        3 -> state.getOrElse(4, 0),
        4 -> state.getOrElse(5, 0),
        5 -> state.getOrElse(6, 0),
        6 -> {
          // Compiler magic I don't understand.
          // Without type-coercion and curly braces, this doesn't work.
          (state.getOrElse(7, 0): Long) + (state.getOrElse(0, 0): Long)
        },
        7 -> state.getOrElse(8, 0),
        8 -> state.getOrElse(0, 0),
      )
    }.values.sum
  }

  override def part1(input: Model): Long = {
    calculateDays(input, 80)
  }

  override def part2(input: Model): Long = {
    calculateDays(input, 256)
  }
}
