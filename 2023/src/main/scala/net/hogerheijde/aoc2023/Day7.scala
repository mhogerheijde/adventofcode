package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser

import scala.collection.MapView
import scala.math.Ordering
import scala.util.Try

object Day7 extends Day[Int, Int]:

  type Model = Seq[(Hand, Int)]

  override def parse(input: String): Model = Parser.parse(game(_))(input).get

  override def part1(input: Model): Int =
    input
      .sortBy(_._1)(part1Ordering)
      .zipWithIndex
      .map { case ((_, bid), rank) => bid * (rank + 1) }
      .sum

  override def part2(input: Model): Int = 0

  val part1Ordering: Ordering[Hand] = (left: Hand, right: Hand) =>
    left.`type`.compare(right.`type`) match
      case 0 =>
        val firstDiffering = left.cards.zip(right.cards).find((left, right) => left != right)
        firstDiffering.map((left, right) => left.compare(right)).getOrElse(0)
      case typeCompare => typeCompare

  case class Hand(cards: Seq[Card]):
    require(cards.size == 5)
    val `type`: Type =
      val histogram: Map[Int, Set[Card]] = Hand.hist(cards)

      if (histogram.contains(5))
        Type.FiveOfAKind
      else if (histogram.contains(4))
        Type.FourOfAKind
      else if (histogram.contains(3) && histogram.contains(2))
        Type.FullHouse
      else if (histogram.contains(3) && !histogram.contains(2))
        Type.ThreeOfAKind
      else if (histogram.contains(2) && histogram(2).size == 2)
        Type.TwoPair
      else if (histogram.contains(2) && histogram(2).size == 1)
        Type.OnePair
      else
        Type.HighCard

  object Hand:
    def hist(cards: Seq[Card]): Map[Int, Set[Card]] = cards //.groupBy(identity)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq
      .groupMapReduce(c => c._2)(q => Set(q._1))(_ ++ _)

  enum Card extends Ordered[Card]:
    case A, K, Q, J, T, _9, _8, _7, _6, _5, _4, _3, _2
    def compare(that: Card): Int = that.ordinal - this.ordinal
  object Card:
    def fromString(card: String): Card = card match
      case "A" => A
      case "K" => K
      case "Q" => Q
      case "J" => J
      case "T" => T
      case "9" => _9
      case "8" => _8
      case "7" => _7
      case "6" => _6
      case "5" => _5
      case "4" => _4
      case "3" => _3
      case "2" => _2


  enum Type extends Ordered[Type]:
    case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard
    def compare(that: Type): Int = that.ordinal - this.ordinal

  private def card[$: P]: P[Card] = P(CharIn("AKQJT98765432").!).map(Card.fromString)
  private def hand[$: P]: P[Hand] = P(card.rep(exactly = 5)).map(Hand.apply)
  private def bid[$: P]: P[(Hand, Int)] = P(hand ~ " " ~ int)
  def game[$: P]: P[Seq[(Hand, Int)]] = P((bid ~ "\n".?).rep)
