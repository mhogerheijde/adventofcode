package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.digit
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.annotation.tailrec

object Day3 extends Day[Long, Long]:

  type Model = Seq[Bank]

  override def parse(input: String): Model = Parser.parse(banks(_))(input).get

  override def part1(input: Model): Long = input.view.map(_.maxJoltage(2)).sum

  override def part2(input: Model): Long = input.view.map(_.maxJoltage(12)).sum

  case class Bank(values: Seq[Int]):
    private def maxJoltage(noOfBatteries: Int, values: Seq[Int]): Long =
      noOfBatteries match
        case x if x < 0 => throw IllegalArgumentException("Number of batteries must be non-negative")
        case 0 => 0
        case 1 => values.max
        case _ =>
          val offset = noOfBatteries - 1
          val max = values.take(values.length - offset).max
          val tail = values.drop(values.indexOf(max) + 1)
          val result = max * (10 ** offset) + maxJoltage(offset, tail)
          result


    def maxJoltage(noOfBatteries: Int): Long = maxJoltage(noOfBatteries, values)


  object Bank:
    def init(values: Int*): Bank = Bank(values)


  def bank[$: P]: P[Bank] = P(digit.rep ~ "\n").map(Bank.apply)
  def banks[$: P]: P[Seq[Bank]] = P(bank.rep)

extension (i: Int)
  def **(exp: Int): Long = Math.pow(i, exp).toLong
