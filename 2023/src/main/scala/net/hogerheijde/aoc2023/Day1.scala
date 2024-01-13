package net.hogerheijde.aoc2023

import scala.util.Try

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.IsInteger

object Day1 extends Day[Int, Int]:
  type Model = Seq[String]

  private[aoc2023] val digits = Seq(
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
  )
  private[aoc2023] val digitString: Seq[String] = Seq(
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
  )

  private def valueOf(digit: String): String =
    if (digits.contains(digit)) { digits.indexOf(digit).toString }
    else if (digitString.contains(digit)) { digitString.indexOf(digit).toString }
    else { throw IllegalArgumentException(s"$digit is not a valid digit.") }

  override def parse(input: String): Model = input.linesIterator.toSeq

  def valueInLine(line: String, possibilities: Seq[String]): Int =
    val leftDigit = possibilities
      .map(digit => (valueOf(digit), line.indexOf(digit)))
      .filterNot { _._2 == -1 }
      .minBy(_._2)

    val rightDigit: (String, Int) = possibilities
      .map(digit => (valueOf(digit), line.lastIndexOf(digit)))
      .filterNot(_._2 == -1)
      .maxBy(_._2)

    Integer.parseInt(leftDigit._1 + rightDigit._1)

  override def part1(input: Model): Int = input.map { line =>
    valueInLine(line, digits)
  }.sum

  override def part2(input: Model): Int = input.map { line =>
    valueInLine(line, digits ++ digitString)
  }.sum