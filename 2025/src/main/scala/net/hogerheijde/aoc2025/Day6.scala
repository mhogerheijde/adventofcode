package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.collection.immutable

object Day6 extends Day[Long, Int]:

  sealed trait Symbol
  object Symbol:
    case class Number(value: Int) extends Symbol
    enum Operation extends Symbol:
      case Add
      case Multiply

  type Model = Grid[Symbol]

  override def parse(input: String): Model = Parser.parse(grid(_))(input).get

  override def part1(input: Model): Long =
    input.columns.view.mapValues(col =>
      col.collectFirst { case x: Symbol.Operation => x } match {
        case Some(Symbol.Operation.Add)      => col.collect { case Symbol.Number(v) => v }.sum.toLong
        case Some(Symbol.Operation.Multiply) => col.collect { case Symbol.Number(v) => v }.foldLeft(1L)(_ * _)
        case None                            => throw RuntimeException(s"There should always be an operation in a column, found none in $col")
      }
    )
      .values
      .foldLeft(0L)(_ + _)

  override def part2(input: Model): Int = 0

  def number[$: P]: P[Symbol.Number] = P(int).map(Symbol.Number.apply)
  def add[$: P]: P[Symbol.Operation] = P("+").map(_ => Symbol.Operation.Add)
  def multiply[$: P]: P[Symbol.Operation] = P("*").map(_ => Symbol.Operation.Multiply)
  def operation[$: P]: P[Symbol.Operation] = P(add | multiply)
  def symbol[$: P]: P[Symbol] = P(number | operation)

  def line[$: P]: P[Seq[Symbol]] = P((" ".rep ~ symbol).rep(min = 1))
  def grid[$: P]: P[Grid[Symbol]] = P((line ~ "\n".?).rep(1)).map { x =>
    Grid(
      x.zipWithIndex.flatMap((symbols, line) =>
        symbols.zipWithIndex.map((symbol, column) =>
          (Coordinate(line, column), symbol)
        )
      )
    )
  }

  extension [T] (g: Grid[T])
    def columns: Map[Int, immutable.Iterable[T]] =
      g.values.groupMap((c, _) => c.column)((_, v) => v)