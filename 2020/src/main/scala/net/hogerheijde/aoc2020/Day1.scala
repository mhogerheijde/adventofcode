package net.hogerheijde.aoc2020

import scala.collection.immutable.Seq

import net.hogerheijde.aoc.util.Day

object Day1 extends Day[Int, Int] {

  type Model = Seq[Int]

  override def name: String = "Day 1"

  override def parse(input: String): Model = {
    input.linesIterator.map(Integer.parseInt).toSeq
  }

  override def part1(input: Model): Int = {
    val candidate = (for {
      part1 <- input
      part2 <- input
    } yield {
      (part1 + part2, part1 * part2)
    }).find(_._1 == 2020)

    candidate.map(_._2).get
  }

  override def part2(input: Model): Int = {
    val candidate = (for {
      part1 <- input
      part2 <- input
      part3 <- input
    } yield {
      (part1 + part2 + part3, part1 * part2 * part3)
    }).find(_._1 == 2020)

    candidate.map(_._2).get
  }
}
