package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.common.parser.Common.long
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day7.Operator.Concat
import net.hogerheijde.aoc2024.Day7.Operator.Plus
import net.hogerheijde.aoc2024.Day7.Operator.Times

import scala.math.pow
import scala.util.Try

object Day7 extends Day[Long, Long]:

  type Model = Seq[Expression]

  override def parse(input: String): Model = Parser.parse(expressions(_))(input).get

  override def part1(input: Model): Long = input
    .filter(_.canBeSatisfied(Seq(Plus, Times)))
    .map(_.testValue).sum

  override def part2(input: Model): Long = input
    .filter(_.canBeSatisfied(Seq(Plus, Times, Concat)))
    .map(_.testValue).sum

  case class Expression(testValue: Long, components: Seq[Long]):
    def resolve(operators: Seq[Operator]): Long =
      components.tail.zipWithIndex.foldLeft(components.head) { case (acc, (next, index)) =>
        operators(index).resolve(acc, next)
      }

    def isSatisfiedBy(operators: Seq[Operator]): Boolean = resolve(operators) == testValue

    def canBeSatisfied(operators: Seq[Operator]): Boolean =
      combinationsWithRepeats(components.size - 1, operators)
        .exists(isSatisfiedBy)



  def expressions[$: P]: P[Seq[Expression]] = P( ("\n".? ~ expression).rep )
  def expression[$: P]: P[Expression] = P(long ~ ": " ~ (long ~ " ".?).rep)
    .map { case (t, s) => Expression(t, s) }


  enum Operator(symbol: String):
    case Plus extends Operator("+")
    case Times extends Operator("*")
    case Concat extends Operator("||")

    def resolve(left: Long, right: Long): Long =
      this match
        case Operator.Plus => left + right
        case Operator.Times => left * right
        case Operator.Concat => (left.toString + right.toString).toLong

    override def toString: String = symbol

  def combinationsWithRepeats[T](resultSize: Int, options: Seq[T]): Iterator[Seq[T]] =
    new Iterator[Seq[T]] {
      private var current = 0
      override def hasNext: Boolean = current < pow(options.length, resultSize).toInt

      override def next(): Seq[T] = {
        val nextValue = current
        current += 1
        val based = Integer.toString(nextValue, options.length) // output nextValue in base "options.length"
        val padded = "0" * (resultSize - based.length) + based
        padded.map(digit => options(digit.asDigit))
      }
    }

