package net.hogerheijde.aoc2021

import scala.collection.immutable.Seq

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc2021.Day1.Direction.Decreased
import net.hogerheijde.aoc2021.Day1.Direction.Increased
import net.hogerheijde.aoc2021.Day1.Direction.Stable

object Day1 extends Day[Int, Int] {

  type Model = Seq[Int]

  override def parse(input: String): Model = {
    input.linesIterator.map(Integer.parseInt).toSeq
  }

  sealed trait Direction

  object Direction {
    case object Increased extends Direction
    case object Decreased extends Direction
    case object Stable extends Direction
  }

  override def part1(input: Model): Int = {
    val directions = input.sliding(2).map {
      case Seq(first, second) if first > second => Decreased
      case Seq(first, second) if first < second => Increased
      case Seq(first, second) if first == second => Stable
      case _ => throw new RuntimeException("Sliding failed!")
    }.toSeq

    directions.count(_ == Increased)
  }

  override def part2(input: Model): Int = {
    part1(input.sliding(3).map {
      case Seq(one, two, three) => one + two + three
      case _ => throw new RuntimeException("Sliding failed!")
    }.toSeq)
  }
}
