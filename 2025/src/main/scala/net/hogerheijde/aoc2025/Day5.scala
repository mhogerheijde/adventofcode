package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.long
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.collection.immutable.NumericRange

object Day5 extends Day[Long, Long]:

  type Model = IMS
  type LongRange = NumericRange.Inclusive[Long]

  override def parse(input: String): Model = Parser.parse(inventory(_))(input).get

  override def part1(input: Model): Long = input.ingredients.foldLeft(0L) { (count, ingredient) =>
    val isFresh = input.ranges.exists { r => r.start <= ingredient && ingredient <= r.end }
    count + (if isFresh then 1L else 0L)
  }

  override def part2(input: Model): Long =
    input.rangesSorted.drop(1).foldLeft(input.rangesSorted.take(1)) { (acc, currentRange) =>
      acc.last match {
        case lastRange if lastRange.end >= currentRange.start =>
          acc.dropRight(1) :+ (lastRange.start to Math.max(lastRange.end, currentRange.end))
        case lastRange if lastRange.end < currentRange.start =>
          acc :+ currentRange
        case _ => throw RuntimeException("Should not happen?")
      }
    }
      .map { r => r.end - r.start + 1 }
      .sum


  case class IMS(ranges: Seq[LongRange], ingredients: Seq[Long]):
    def rangesSorted: Seq[LongRange] = ranges.sortBy(_.start)

  def range[$: P]: P[LongRange] = P(long ~ "-" ~ long ~ "\n").map { NumericRange.Inclusive[Long](_, _, 1) }
  def ranges[$: P]: P[Seq[LongRange]] = P(range.rep())

  def ingredients[$: P]: P[Seq[Long]] = P((long ~ "\n").rep())

  def inventory[$: P]: P[IMS] = P(ranges ~ "\n" ~ ingredients).map { IMS(_, _) }