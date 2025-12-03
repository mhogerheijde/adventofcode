package net.hogerheijde.aoc.common.parser

import fastparse.NoWhitespace.*
import fastparse.*
import net.hogerheijde.aoc.common.model.Coordinate
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.common.model
import net.hogerheijde.aoc.common.model.Grid

object Common:
  def digit[$: P]: P[Int] = P(CharIn("0-9").rep(exactly = 1).!).map(_.toInt)
  def alphaLower[$: P]: P[String] = P(CharIn("a-z").rep(min = 1).!)
  def intSeq[$: P]: P[IndexedSeq[Int]] = P((int ~ ("," | ";").? ~ " ".rep.?).rep).map(_.toIndexedSeq)

  def int[$: P]: P[Int] = P("-".? ~ uint).map(_.toInt)
  def uint[$: P]: P[Int] = P(CharIn("0-9").rep(min = 1).!).map(_.toInt)

  def long[$: P]: P[Long] = P("-".? ~ ulong).map(_.toLong)
  def ulong[$: P]: P[Long] = P(CharIn("0-9").rep(min = 1).!).map(_.toLong)

  def coordinate[$: P]: P[Coordinate] = P((int ~ "," ~ " ".rep ~ int).map(t => Coordinate(t._1, t._2)))
  def coordinates[$: P]: P[Seq[Coordinate]] = P((coordinate ~ "\n").rep)


object TokenGrid:
  type IndexedToken[T] = (Int, T)
  type TokenParser[T] = P[IndexedToken[T]]

  private def char[$: P]: TokenParser[Char] = P(Index ~ CharPred(_ != '\n').!).map { case (x, y) => (x, y.charAt(0)) }

  def charGrid[$: P]: P[Grid[Char]] = grid(char)
  def grid[T, $: P](parser: => TokenParser[T]): P[Grid[T]] = P(row(parser).rep ~ lastRow(parser))
    .map { case (x, y) => x :+ y }
    .map { case (x: Seq[Seq[IndexedToken[T]]]) =>
      x.zipWithIndex.foldLeft(Grid.empty[T]) { case (grid, (row, rowNumber)) =>
        row.foldLeft(grid) { case (subGrid, (column, token)) =>
          subGrid.add(Coordinate(vertical = rowNumber, horizontal = column), token)
        }
      }
    }

  private def indexedTokens[T, $: P](parser: => TokenParser[T]): P[Seq[IndexedToken[T]]] =
    P(Index ~ parser.rep).map {
      case (startOfLine, tokens) => tokens.map { case (index, token) => (index - startOfLine, token) }
    }
  private def row[T, $: P](parser: => TokenParser[T]): P[Seq[IndexedToken[T]]] = P(indexedTokens(parser) ~ "\n").map { (x: Seq[(Int, T)]) => x}
  private def lastRow[T, $: P](parser: => TokenParser[T]): P[Seq[IndexedToken[T]]] = P(indexedTokens(parser) ~ End)