package net.hogerheijde.aoc2015.day2

import scala.io.Source
import scala.collection.immutable.IndexedSeq


object Day2 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("net/hogerheijde/aoc2015/day2.txt").mkString

    val boxes = parse(input)

    val result1 = part1(boxes)
    println(s"Day 1; part 1: $result1")
    val result2 = part2(boxes)
    println(s"Day 1; part 2: $result2")
  }

  def parse(input: String): IndexedSeq[Box] = {
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

  case class Box(length: Int, width: Int, height: Int) {

    val wrappingPaper: Int = {
      val sides = Seq(length * width, width * height, height * length)
      sides.min + sides.foldLeft(0) { (total, side) => total + 2*side}
    }

    val ribbon: Int = {
      val dimensions = Seq(length, width, height).sorted
      val around = dimensions.init.foldLeft(0) { (total, side) => total + 2*side }
      val bow = dimensions.product
      bow + around
    }
  }


}
