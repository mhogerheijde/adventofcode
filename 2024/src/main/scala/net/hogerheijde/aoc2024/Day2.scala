package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Safety.Safe
import net.hogerheijde.aoc2024.Safety.SafeWithSkip
import net.hogerheijde.aoc2024.Safety.Unsafe

import scala.collection.immutable.Seq
import java.lang.Math.abs
import scala.util.Try

object Day2 extends Day[Int, Int]:
  type Model = Seq[Report]

  override def parse(input: String): Model = Parser.parse(reports(_))(input).get

  override def part1(input: Model): Int = input.count(_.isSafe)

  override def part2(input: Model): Int = input.count(_.canBeDampened)

  def reports[$: P]: P[Model] = P((report ~ "\n".?).rep)
  def report[$: P]: P[Report] = P((int ~ " ").rep ~ int).map { case (xs, x) => Report(xs.toSeq :+ x) }


case class Report(levels: Seq[Int]):
  val isSafe: Boolean =
    (levels.strictlyIncreasing || levels.strictlyDecreasing) &&
      levels.sliding(2).forall { case Seq(x, y) => abs(x - y).isBetween(1, 3) }

  def canBeDampened: Boolean =
    isSafe ||
      levels.indices.map { i => Report(levels.slice(0, i) ++ levels.slice(i + 1, levels.size)) }.exists(_.isSafe)

enum Safety:
  case Safe
  case SafeWithSkip
  case Unsafe


extension (s: Seq[Int])
  def strictlyIncreasing: Boolean = s.size <= 1 || s.sliding(2).forall { case Seq(x, y) => x < y }
  def strictlyDecreasing: Boolean = s.size <= 1 || s.sliding(2).forall { case Seq(x, y) => x > y }

extension (i: Int)
  def isBetween(lower: Int, upper: Int): Boolean = i >= lower && i <= upper