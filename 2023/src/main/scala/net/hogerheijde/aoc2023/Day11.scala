package net.hogerheijde.aoc2023

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day

import scala.util.Try

object Day11 extends Day[Int, Long]:

  type Model = Seq[Coordinate]

  override def parse(input: String): Model = universes(input)

  override def part1(input: Model): Int =
    expand(input).pairs.map { (a, b) => a.manhattan(b) }.sum

  override def part2(input: Model): Long =
    expand(input, 1000000).pairs.map { (a, b) => a.manhattan(b).toLong }.sum

  def universes(input: String): Seq[Coordinate] =
    input
      .linesIterator
      .zipWithIndex
      .flatMap { (scanLine, row) =>
        val u = scanLine.zipWithIndex.filter { (x, _) => x == '#'}
        u.map { (_, col) => Coordinate(vertical = row, horizontal = col) }
      }
      .toSeq

  def expand(u: Seq[Coordinate], amount: Int = 2): Seq[Coordinate] =
    val maxRow = u.maxBy(_.row).row
    val maxCol = u.maxBy(_.column).column
    val missingRows = (0 to maxRow).diff(u.map(_.row)).sorted
    val missingColumns = (0 to maxCol).diff(u.map(_.column)).sorted
    u.map { c =>
      Coordinate(
        vertical = c.row + ((amount-1) * missingRows.count(_ < c.row)),
        horizontal = c.column + ((amount-1) * missingColumns.count(_ < c.column)),
      )
    }

  extension [T] (s: Seq[T])
    def pairs: Seq[(T, T)] = s.combinations(2).map { (q: Seq[T]) => (q.head, q(1)) }.toSeq

  extension (c: Coordinate)
    def manhattan(o: Coordinate): Int = Math.abs(c.vertical - o.vertical) + Math.abs(c.horizontal - o.horizontal)