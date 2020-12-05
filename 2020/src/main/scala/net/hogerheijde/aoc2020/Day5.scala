package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Day

object Day5 extends Day[Int, Int] {

  type Model = Seq[Int]

  override def parse(input: String): Day5.Model = input
    .linesIterator
    .map { line =>
      (
        line
          .take(7)
          .replace("F", "0")
          .replace("B", "1"),
        line
          .drop(7)
          .replace("L", "0")
          .replace("R", "1"),
      )
    }
    .map { ident =>
      (Integer.parseInt(ident._1, 2) * 8) +
        Integer.parseInt(ident._2, 2)
    }
    .toSeq

  override def part1(input: Day5.Model): Int = input.max

  override def part2(input: Day5.Model): Int = input
    .sorted
    .sliding(2)
    .find { seats => seats (0) == seats(1) - 2 }
    .map { seats => seats(0) + 1 }
    .get


}
