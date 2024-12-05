package net.hogerheijde.aoc2024

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Direction
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.TokenGrid
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.annotation.targetName

object Day4 extends Day[Int, Int]:

  type Model = Grid[Char]

  override def parse(input: String): Model = Parser.parse(TokenGrid.charGrid(_))(input).get

  override def part1(input: Model): Int =
    input.map { case (x, _) => input.countXmas(x) }.values.values.sum

  override def part2(input: Model): Int = input.values.count { case (c, _) => input.`x-mas`(c) }

  private val mas = Seq('M', 'A', 'S').map(Some(_))
  private val masRev = mas.reverse
  extension(g: Model)

    def `x-mas`(c: Coordinate): Boolean =
      val ╱ = Seq(g.values.get(c.transpose.leftUp), g.values.get(c), g.values.get(c.transpose.rightDown))
      val ╲ = Seq(g.values.get(c.transpose.rightUp), g.values.get(c), g.values.get(c.transpose.leftDown))
      (╱ == mas || ╱ == masRev) && (╲ == mas || ╲ == masRev)

    def xmas(c: Coordinate, d: Direction, expect: Seq[Char]= Seq('X', 'M', 'A', 'S')): Boolean =
      expect match
        case Seq() => true
        case h +: tail =>
          if (g.values.get(c).contains(h))
            g.xmas (c.transpose.directionOf(d), d, tail)
          else
            false

    def countXmas(c: Coordinate): Int = Direction.values.count(g.xmas(c, _))