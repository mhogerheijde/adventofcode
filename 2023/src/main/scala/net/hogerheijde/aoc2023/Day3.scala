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

object Day3 extends Day[Long, Long]:

  type Model = Grid[Token]

  override def parse(input: String): Model =
    Parser.parse(Day3.grid(_))(input).get

  override def part1(input: Model): Long = input.partNumbers.map(_.value).sum.toLong

  override def part2(input: Model): Long = 0L

  sealed trait Token
  case class Symbol(symbol: Char) extends Token:
    override val toString: String = symbol.toString

  case class Value(value: Int) extends Token:
    override val toString: String = value.toString

    override def equals(obj: Any): Boolean =
      obj match
        case v: Value => this.value == v.value
        case i: Int => this.value == i
        case _ => false


  def value[$: P]: P[Value] = P(Common.uint).map(Value.apply)
  def symbol[$: P]: P[Symbol] = P( !"\n" ~ AnyChar.!).map(s => Symbol(s.head))

  def token[$: P]: P[(Int, Token)] = P(".".rep(min=0) ~ Index ~ (value | symbol) ~ ".".rep(min = 0))
  def tokens[$: P]: P[Seq[(Int, Token)]] = P(Index ~ token.rep).map { (startOfLine, tokens) =>
    tokens.map { (index, token) => (index - startOfLine, token) }
  }

  def line[$: P]: P[Seq[(Int, Token)]] = P(tokens ~ "\n")
  def lastLine[$: P]: P[Seq[(Int, Token)]] = P(tokens ~ End)

  def grid[$: P]: P[Grid[Token]] = P(line.rep ~ lastLine)
    .map { (x, y) => x :+ y }
    .map {
      _.zipWithIndex.foldLeft(Grid.empty[Token]) { case (grid, (row, rowNumber)) =>
        row.foldLeft(grid) { case (subGrid, (column, token)) =>
          subGrid.add(Coordinate(vertical = rowNumber, horizontal = column), token)
        }
      }
    }


  def findAdjacent(root: Coordinate, value: Day3.Value): Set[Coordinate] =
    val endCoordinate = root add(0, value.toString.length - 1)
    val caps = Set(
      root.transpose.leftUp,
      root.transpose.left,
      root.transpose.leftDown,
      endCoordinate.transpose.rightUp,
      endCoordinate.transpose.right,
      endCoordinate.transpose.rightDown,
    )

    val middle: Set[Coordinate] = Range(0, value.toString.length).flatMap { offset =>
      val c2 = root add(0, offset)
      Set(c2.transpose.up, c2.transpose.down)
    }.toSet

    caps ++ middle

  extension (grid: Grid[Token])
    def mostLeft = grid.values.keys.minBy { _.column }

//    def charAt(c: Coordinate): Char =
//      grid.values.get(c) match
//        case Some(Symbol(s)) => s
//        case Some(Par)
//        case None => if (c.column > mostLeft)

//    def display: String =

    def isPartnumber(c: Coordinate): Boolean =
      grid.values.get(c) match
        case Some(v: Value) => findAdjacent(c, v).exists { a => grid.values.get(a).exists(_.isInstanceOf[Symbol]) }
        case _ => false

    def partNumbers: Seq[Value] =
      grid.values
        .filter((c, _) => isPartnumber(c))
        .toSeq
        .sortBy(_._1) // for debugging
        .collect { case (_: Coordinate, v: Value) => v }
