package net.hogerheijde.aoc2015

import net.hogerheijde.aoc.util.Day

object Day10 extends Day[Int, Unit] {
  override type Model = String

  override def parse(input: String): Model = input

  override def part1(input: Model): Int = Range(0, 40).foldLeft(input) { case (acc, i) =>
    println(s"Round $i")
    lookAndSay(acc)
  }.length

  override def part2(input: Model): Unit = ()

  def lookAndSay(in: String): String = in.drop(1).foldLeft(List((in.head, 1))) { case (acc, next) =>
    if (acc.last._1 == next) {
      acc.init :+ ((next, acc.last._2 + 1))
    } else {
      acc :+ ((next, 1))
    }
  }.map { case (char, count) =>
    s"$count$char"
  }.mkString("")
}
