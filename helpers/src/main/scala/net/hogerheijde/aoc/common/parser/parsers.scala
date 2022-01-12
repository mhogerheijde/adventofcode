package net.hogerheijde.aoc.common.parser

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.common.model

object Common {

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt)
  def alphaLower[_: P]: P[String] = P(CharIn("a-z").rep(1).!)


  def intSeq[_: P]: P[IndexedSeq[Int]] = P((int ~ ("," | ";").? ~ " ".rep.?).rep).map(_.toIndexedSeq)

  def int[_: P]: P[Int] = P(("-".? ~ CharIn("0-9").rep(1)).!.map(_.toInt))
  def coordinate[_: P]: P[Coordinate] = P((int ~ "," ~ " ".rep ~ int).map(t => Coordinate(t._1, t._2)))
}

/**
  * Parses a rectangular grid of digits 0-9.
  * All lines must be equal length.
  */
object DigitGrid {
  private def digit[_: P]: P[(Int, Byte)] = P(Index ~ CharIn("0-9").!).map { case (column, char) => (column, char.toByte) }
  private def row[_: P]: P[Seq[(Int, Byte)]] = P(digit.rep)
  def grid[_: P]: P[model.DigitGrid] = P((row ~ "\n").rep).map { rows =>
    model.DigitGrid(rows.zipWithIndex.flatMap { case (depths, row) =>
      depths.map { case (column, depth) => Coordinate(column - (row * (depths.size + 1)), row) -> depth }
    }.toMap)
  }
}