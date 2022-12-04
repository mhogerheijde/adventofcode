package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.util.Day

object Day4 extends Day[Int, Int]{
  override type Model = Seq[(Range, Range)]

  override def parse(input: String): Model = input.linesIterator
    .map { line =>
      val pair = line
        .split(",", 2) // Array("1-2", "3-4")
        .map { range =>
          range.split("-", 2).toSeq match {
            case Seq(start, end) => Range.inclusive(Integer.parseInt(start, 10), Integer.parseInt(end, 10))
            case _ => throw new RuntimeException(s"Cannot create range for $range")
          }
        }
        .toSeq

      pair match {
        case Seq(left, right) => (left, right)
        case _ => throw new RuntimeException(s"Cannot create range for $line")
      }
    }
    .toSeq

  def overlap: ((Range, Range)) => Boolean = { case (r1, r2) =>
    !(r1.end < r2.start || r1.start > r2.end)
  }

  override def part1(input: Model): Int = input.count { case (left, right) =>
    left.containsSlice(right) || right.containsSlice(left)
  }

  override def part2(input: Model): Int = input.count(overlap)
}
