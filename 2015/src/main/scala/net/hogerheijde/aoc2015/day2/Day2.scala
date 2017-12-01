package net.hogerheijde.aoc2015.day2


import net.hogerheijde.aoc2015.util.Day

import scala.collection.immutable.IndexedSeq

object Day2 extends Day[IndexedSeq[Box], Int, Int] {
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 2"
  override def parse: String => IndexedSeq[Box] = { input =>
    (input.split("\n") map { line =>
      // l, w, h per line
      val parts = line.split("x").toIndexedSeq map Integer.parseInt // Ignore the fact that this might throw
      Box(
        length = parts(0),
        width = parts(1),
        height = parts(2))
    }).toIndexedSeq
  }

  def part1(input: IndexedSeq[Box]): Int = {
    input.foldLeft(0) { (total, box) => total + box.wrappingPaper }
  }

  def part2(input: IndexedSeq[Box]): Int = {
    input.foldLeft(0) { (total, box) => total + box.ribbon }
  }
}
