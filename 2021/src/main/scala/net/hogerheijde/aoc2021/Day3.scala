package net.hogerheijde.aoc2021

import scala.annotation.tailrec

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser.standardLineSplit

object Day3 extends Day[Int, Int] {
  override type Model = Seq[Seq[Boolean]]

  override def parse(input: String): Model = standardLineSplit(input)
    .map(bits =>
      bits.map {
        case '0' => false
        case '1' => true
        case char => throw new RuntimeException(s"Not expecting $char")
      }
    )

  def toNumber(seq: Seq[Boolean]): Int = {
    seq.reverse.foldLeft((0, 0)) {
      case ((acc, power), true) => (acc + Math.pow(2, power).toInt, power + 1)
      case ((acc, power), false) => (acc, power +1)
    }._1
  }

  def bitCriteria(input: Model, invert: Boolean = false): Seq[Boolean] = {
    val zero: Seq[Int] = Range(0, input.head.size).map(_ => 0)
    input.foldLeft(zero) { (acc, next) =>
      next
        .zip(acc)
        .map {
          case (false, tally) => tally - 1
          case (true, tally) => tally + 1
        }
    }.map {
      case i if i > 0 => !invert
      case i if i < 0 => invert
      case i if i == 0 => !invert
    }
  }

  @tailrec
  def oxygenRating(input: Model, position: Int = 0): Int = {
    input match {
      case Seq(single) => toNumber(single)
      case list =>
        val criteria = bitCriteria(list)
        val criterium = criteria(position)
        val result = list.filter(number => number(position) == criterium)
        oxygenRating(result, position + 1)
    }
  }

  @tailrec
  def co2Rating(input: Model, position: Int = 0): Int = {
    input match {
      case Seq(single) => toNumber(single)
      case list =>
        val criteria = bitCriteria(list, invert = true)
        val criterium = criteria(position)
        val result = list.filter(number => number(position) == criterium)
        co2Rating(result, position + 1)
    }
  }

  override def part1(input: Model): Int = {
    val `γ-rate`: Seq[Boolean] = bitCriteria(input)
    val `ε-rate` = `γ-rate`.map(b => !b)
    toNumber(`ε-rate`) * toNumber(`γ-rate`)
  }

  override def part2(input: Model): Int = {
    oxygenRating(input) * co2Rating(input)
  }
}
