package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.Common
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.collection.immutable
import scala.util.Try

object Day3 extends Day[Int, Int]:

  type Model = Parts

  override def parse(input: String): Model =
    Parser.parse(Day3.grid(_))(input).get

  override def part1(input: Model): Int = input.partNumbers.map(_.value).sum

  override def part2(input: Model): Int = input.gearRatios.values.sum

  sealed trait Token:
    def id: Int
    def length: Int

    override def equals(obj: Any): Boolean =
      obj match
        case t: Token => id == t.id
        case _ => false

  case class Symbol(symbol: Char, id: Int) extends Token:
    override val length = 1
    val isGear = '*' == symbol
    override val toString: String = s"$id: $symbol"

  case class Value(value: Int, id: Int) extends Token:
    override val length: Int = value.toString.length
    override val toString: String = s"$id: $value"


  def value[$: P]: P[Value] = P(Index ~ Common.uint).map((i, v) => Value(v, i))
  def symbol[$: P]: P[Symbol] = P( !"\n" ~ Index ~ AnyChar.!).map((i, s) => Symbol(s.head, i))

  def token[$: P]: P[(Int, Token)] = P(".".rep(min=0) ~ Index ~ (value | symbol) ~ ".".rep(min = 0))
  def tokens[$: P]: P[Seq[(Int, Token)]] = P(Index ~ token.rep).map { (startOfLine, tokens) =>
    tokens.map { (index, token) => (index - startOfLine, token) }
  }

  def line[$: P]: P[Seq[(Int, Token)]] = P(tokens ~ "\n")
  def lastLine[$: P]: P[Seq[(Int, Token)]] = P(tokens ~ End)

  def grid[$: P]: P[Parts] = P(line.rep ~ lastLine)
    .map { (x, y) => x :+ y }
    .map { (x: Seq[Seq[(Int, Token)]]) =>
      Parts(
        x.zipWithIndex.foldLeft(Grid.empty[Token]) { case (grid, (row, rowNumber)) =>
          row.foldLeft(grid) { case (subGrid, (column, token)) =>
            subGrid.add(Coordinate(vertical = rowNumber, horizontal = column), token)
          }
        }
      )
    }

  def adjacentCoordinates(root: Coordinate, token: Day3.Token): Set[Coordinate] =
    val endCoordinate = root.add(0, token.length - 1)
    val caps = Set(
      root.transpose.leftUp,
      root.transpose.left,
      root.transpose.leftDown,
      endCoordinate.transpose.rightUp,
      endCoordinate.transpose.right,
      endCoordinate.transpose.rightDown,
    )

    val middle: Set[Coordinate] = Range(0, token.length).flatMap { offset =>
      val c2 = root add(0, offset)
      Set(c2.transpose.up, c2.transpose.down)
    }.toSet

    caps ++ middle

  case class Parts(grid: Grid[Token]):

    private def isPartNumber(c: Coordinate): Boolean =
      grid.values.get(c) match
        case Some(_: Value) => adjacentSymbols(c).nonEmpty
        case _ => false

    private def isGearRatio(c: Coordinate): Boolean =
      grid.values.get(c) match
        case Some(_: Value) => adjacentSymbols(c).map(_.symbol).contains('*')
        case _ => false


    private def adjacentSymbols(c: Coordinate): Set[Symbol] =
      grid.values.get(c) match
        case Some(v: Value) =>
          adjacentCoordinates(c, v)
            .foldLeft(Set.empty[Symbol]) { (symbols, a) =>
              grid.values.get(a) match
                case Some(s: Symbol) => symbols + s
                case _ => symbols
            }
        case _ => Set()


    val partNumbers: Seq[Value] =
      grid.values
        .collect { case (c: Coordinate, v: Value) if isPartNumber(c) => v }
        .toSeq

    val gearRatios: Map[Symbol, Int] =
      grid.values
        .collect { case (c: Coordinate, v: Value) => (v, adjacentSymbols(c).filter(_.isGear)) }
        .toSeq
        .flatMap { case (k, gears) => gears.map(g => (g, k)) }
        .groupMapReduce(_._1)(q => Set(q._2))((q, r) => q ++ r)
        .filter((_, vs) => vs.size >= 2)
        .map((g, vs) => (g, vs.toSeq.map(_.value).product))

    val gears: Seq[Token] =
      grid.values
        .collect { case (_: Coordinate, t: Symbol) if t.isGear => t }
        .toSeq