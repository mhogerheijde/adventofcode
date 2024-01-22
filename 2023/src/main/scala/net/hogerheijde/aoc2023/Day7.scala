package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day7.Card
import net.hogerheijde.aoc2023.Day7.Card.J
import net.hogerheijde.aoc2023.Day7.Card.ordinalPartTwoMapping
import net.hogerheijde.aoc2023.Day7.Type.FiveOfAKind
import net.hogerheijde.aoc2023.Day7.Type.FourOfAKind
import net.hogerheijde.aoc2023.Day7.Type.FullHouse
import net.hogerheijde.aoc2023.Day7.Type.HighCard
import net.hogerheijde.aoc2023.Day7.Type.OnePair
import net.hogerheijde.aoc2023.Day7.Type.ThreeOfAKind
import net.hogerheijde.aoc2023.Day7.Type.TwoPair

import scala.collection.MapView
import scala.collection.immutable.Map
import scala.math.Ordering
import scala.util.Try

object Day7 extends Day[Int, Int]:

  type Model = Seq[(Hand, Int)]
  type Histogram = Map[Int, Set[Card]]

  override def parse(input: String): Model = Parser.parse(game(_))(input).get

  override def part1(input: Model): Int =
    input
      .sortBy(_._1)(part1Ordering)
      .zipWithIndex
      .map { case ((_, bid), rank) => bid * (rank + 1) }
      .sum

  override def part2(input: Model): Int =
    input
      .sortBy(_._1)(part2Ordering)
      .zipWithIndex
      .map { case ((_, bid), rank) => bid * (rank + 1) }
      .sum

  val part1Ordering: Ordering[Hand] = (left: Hand, right: Hand) =>
    left.`type`.compare(right.`type`) match
      case 0 =>
        val firstDiffering = left.cards.zip(right.cards).find((left, right) => left != right)
        firstDiffering.map((left, right) => left.comparePartOne(right)).getOrElse(0)
      case typeCompare => typeCompare

  val part2Ordering: Ordering[Hand] = (left: Hand, right: Hand) =>
    left.type2.compare(right.type2) match
      case 0 =>
        val firstDiffering = left.cards.zip(right.cards).find((left, right) => left != right)
        firstDiffering.map((left, right) => left.comparePartTwo(right)).getOrElse(0)
      case typeCompare => typeCompare

  case class Hand(cards: Seq[Card]):
    require(cards.size == 5)

    val histogram: Histogram = Hand.hist(cards)

    private def hasPairs(amount: Int): Boolean = histogram.get(2).exists(_.size == amount)
    private def exactly(n: Int, c: Card): Boolean = histogram.get(n).exists(_.contains(c))
    private def any(c: Card): Boolean = histogram.exists((_ , cards) => cards.contains(c))
    private val hasSingle: Boolean = histogram.contains(1)
    private val hasAnyPairs: Boolean = histogram.contains(2)
    private val hasTriplet: Boolean = histogram.contains(3)
    private val hasQuadruple: Boolean = histogram.contains(4)
    private val hasQuintuple: Boolean = histogram.contains(5)

    lazy val `type`: Type =
      if (hasQuintuple)
        FiveOfAKind
      else if (hasQuadruple)
        FourOfAKind
      else if (hasTriplet && hasPairs(1))
        FullHouse
      else if (hasTriplet && !hasAnyPairs)
        ThreeOfAKind
      else if (hasPairs(2))
        TwoPair
      else if (hasPairs(1) && !hasTriplet) // Technically the FullHouse above should make it so we never get here...
        OnePair
      else
        HighCard

    lazy val type2: Type =
      if (
        hasQuintuple
          || (hasQuadruple && exactly(1, J))
          || (hasTriplet && exactly(2, J))
          || (hasPairs(1) && exactly(3, J))
          || exactly(4, J) // hasQuintuple will fire if there are 5 J's
      ) FiveOfAKind

      else if (
        hasQuadruple
          || (hasTriplet && exactly(1, J)) // XXXJZ
          || (hasPairs(2) && exactly(2, J)) // two pairs, one of which J
          || (hasSingle && exactly(3, J)) // JJJXY. Can't be JJJXX; no single would exists
      ) FourOfAKind

      else if (
        (hasTriplet && hasPairs(1) && !exactly(2, J) && !exactly(3, J)) //XXXYY
          || (hasPairs(2) && exactly(1, J))
      ) FullHouse

      else if (
        (hasTriplet && !hasPairs(1) && !exactly(3, J)) // XXXYZ if JJJYZ, this would be a FourOfAKind
          || (hasPairs(1) && exactly(1, J)) // XXJYZ
          || (exactly(2, J) && histogram.get(1).exists(_.size == 3)) // JJXYZ
      ) ThreeOfAKind

      else if (hasPairs(2) && !exactly(2, J) && !exactly(1, J))
        // Any pair with a J becomes a ThreeOfAKind
        TwoPair
      else if (
        (hasPairs(1) && !any(J))
        || (any(J) && histogram.get(1).exists(_.size == 5))
      )
        OnePair
      else if (!any(J) && histogram.get(1).exists(_.size == 5))
        // HighCard with a Joker, automatically becomes a pair
        HighCard
      else
        throw new IllegalStateException("Unrecognised type!")

  object Hand:
    def hist(cards: Seq[Card]): Histogram = cards
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq
      .groupMapReduce(c => c._2)(q => Set(q._1))(_ ++ _)

  enum Card:
    case A, K, Q, J, T, _9, _8, _7, _6, _5, _4, _3, _2
    def comparePartOne(that: Card): Int = that.ordinal - this.ordinal
    def comparePartTwo(that: Card): Int =
      ordinalPartTwoMapping(that) - ordinalPartTwoMapping(this)

  object Card:
    private def ordinalPartTwoMapping(c: Card): Int =
      c match
        case A | K | Q => c.ordinal
        case J => _2.ordinal
        case _ => c.ordinal -1

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
