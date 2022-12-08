package net.hogerheijde.aoc2022

import scala.util.Try

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.IsInteger

object Day1 extends Day[Int, Int] {

  type Model = Seq[Seq[Int]]

  override def parse(input: String): Model = {
    val inventory = input.linesIterator.foldLeft(
      (Seq.empty[Seq[Int]], Seq.empty[Int])
    ) { case ((acc, currentElf), next) =>
      next match {
        case a if a.isBlank => (acc :+ currentElf, Seq.empty[Int])
        case IsInteger(i) => (acc, currentElf :+ i)
        case _ => throw new RuntimeException("Found non-caloric snack.")
      }
    }
    inventory._1 :+ inventory._2 // need to add the last elves inventory
  }
  override def part1(input: Model): Int = input.map { _.sum }.max

  override def part2(input: Model): Int = input.map { _.sum }.sorted(Ordering[Int].reverse).take(3).sum
}
