package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.common.parser.Common.uint
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day3.Value

import scala.util.Try

object Day4 extends Day[Int, Int]:

  type Model = Seq[Card]

  override def parse(input: String): Model = Parser.parse(cards(_))(input).get

  override def part1(input: Model): Int = input.map(points).sum

  override def part2(input: Model): Int =
    val pointsByCard: Map[Card, Int] = input.map(c => (c, winningNumbers(c))).toMap
    val initialCardCount: Map[Int, Int] = input.map(c => (c.number, 1)).toMap

    input
      .foldLeft(initialCardCount) { case (runningTotal, card) =>
        Range.inclusive(card.number + 1, card.number + pointsByCard(card))
          .foldLeft(runningTotal) { (subTotal, nextCard) =>
            subTotal.updated(nextCard, subTotal(nextCard) + subTotal(card.number))
          }
      }
      .values
      .sum

  case class Card(
    number: Int,
    winning: Set[Int],
    actual: Seq[Int],
  )

  def winningNumbers(card: Card): Int = card.actual.collect { case n if card.winning(n) => n }.size
  def points(card: Card): Int = Math.pow(2, winningNumbers(card) - 1).toInt

  private def cards[$: P]: P[Seq[Card]] = P((card ~ "\n".?).rep)
  private def numbers[$: P]: P[Seq[Int]] = P((" ".rep(min = 0) ~ uint).rep(min=1))
  private def card[$: P]: P[Card] = P("Card" ~ " ".rep(min=1) ~ uint ~ ": " ~ numbers ~ " | " ~ numbers)
    .map((n, winning, got) => Card(n, winning.toSet, got))

