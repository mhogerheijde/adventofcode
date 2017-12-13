package net.hogerheijde.aoc2017.day10

import net.hogerheijde.aoc2017.Day

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq



object Day10 extends Day[String, Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 10"
  override def parse: String => String = identity
  override def part1(input: String): Int = {
    val lengths = input.split(",").map(_.toInt).toIndexedSeq

    val start = Running(Range(0, 256), 0, 0, lengths)
    val resolved = resolve1(start)

    resolved.unRotate.take(2).product
  }

  override def part2(input: String): Int = {
    val lengths = input.split("").map(_.toCharArray.head.toInt)

    val postfix = IndexedSeq(17, 31, 73, 47, 23)

  }

  @tailrec
  def resolve1(input: State): Ended = {
    input match {
      case e: Ended => e
      case r: Running => resolve1(r.next)
    }
  }


}

