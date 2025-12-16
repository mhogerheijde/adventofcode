package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.collection.immutable

object Day6 extends Day[Long, Long]:

  sealed trait Symbol
  object Symbol:
    case class Number(value: Int) extends Symbol
    enum Operation extends Symbol:
      case Add
      case Multiply

  type Model = String

  override def parse(input: String): Model = input

  override def part1(input: Model): Long =
    Parser.parse(grid(_))(input)
      .get
      .columns
      .view.mapValues(col =>
        col.collectFirst { case x: Symbol.Operation => x } match {
          case Some(Symbol.Operation.Add)      => col.collect { case Symbol.Number(v) => v }.sum.toLong
          case Some(Symbol.Operation.Multiply) => col.collect { case Symbol.Number(v) => v }.foldLeft(1L)(_ * _)
          case None                            => throw RuntimeException(s"There should always be an operation in a column, found none in $col")
        }
      )
        .values
        .safeSum

  override def part2(input: Model): Long =
    ColumnReader(input)
      .extractExpressions()
      .map(_.solve)
      .safeSum


  class ColumnReader(input: String):
    private var position = 0;
    private val lines: IndexedSeq[String] = input.linesIterator.toIndexedSeq
    private val columns = lines.init
    private val operators = lines.last

    def nextColumn(): Option[(Long, Option[Symbol.Operation])] =
      if position >= lines(0).length then
        None
      else
        val num = columns.map(_.charAt(position)).foldLeft("")(_ + _).trim
        val operator = operators.charAt(position)
        position += 1

        (num, operator) match
          case ("", ' ') =>
            None
          case (n, o) =>
            val number = n.toLong
            val operation = o match {
              case '+' => Some(Symbol.Operation.Add)
              case '*' => Some(Symbol.Operation.Multiply)
              case ' ' => None
            }
            Some((number, operation))


    def nextExpression(partial: Option[Expression] = None): Option[Expression] =
      nextColumn() match
        case None => partial
        case Some(long, None) =>
          val p: Option[Expression] = partial.map(x => x :+ long)
          nextExpression(p)
        case Some(long, Some(operation)) =>
          nextExpression(Some(Expression(long, operation)))

    def extractExpressions(): Seq[Expression] =
      Iterator
        .continually(nextExpression())
        .takeWhile(_.isDefined)
        .flatten
        .toSeq

  case class Expression(
    values: Seq[Long],
    operation: Symbol.Operation,
  ):
    def `:+`(l: Long): Expression = copy(values = values :+ l)
    def solve: Long = operation match
      case Symbol.Operation.Add      => values.safeSum
      case Symbol.Operation.Multiply => values.safeProduct

  object Expression:
    def apply(l: Long, o: Symbol.Operation): Expression = Expression(Seq(l), o)


  def number[$: P]: P[Symbol.Number] = P(int).map(Symbol.Number.apply)
  def add[$: P]: P[Symbol.Operation] = P("+").map(_ => Symbol.Operation.Add)
  def multiply[$: P]: P[Symbol.Operation] = P("*").map(_ => Symbol.Operation.Multiply)
  def operation[$: P]: P[Symbol.Operation] = P(add | multiply)
  def symbol[$: P]: P[Symbol] = P(number | operation)

  def line[$: P]: P[Seq[Symbol]] = P((" ".rep ~ symbol).rep(min = 1))
  def grid[$: P]: P[Grid[Symbol]] = P((line ~ (" ".rep ~ "\n")).rep(1)).map { x =>
    Grid(
      x.zipWithIndex.flatMap((symbols, line) =>
        symbols.zipWithIndex.map((symbol, column) =>
          (Coordinate(line, column), symbol)
        )
      )
    )
  }

  extension (l: Iterable[Long])
    def safeSum: Long = l.foldLeft(0L)(_ + _)
    def safeProduct: Long = l.foldLeft(1L)(_ * _)

  extension [T] (g: Grid[T])
    def columns: Map[Int, immutable.Iterable[T]] =
      g.values.groupMap((c, _) => c.column)((_, v) => v)
