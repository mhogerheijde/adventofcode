package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import fastparse.ParserInputSource.fromReadable
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.TokenGrid
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2025.Day4.Tile.Empty
import net.hogerheijde.aoc2025.Day4.Tile.PaperRoll

import scala.annotation.tailrec

object Day4 extends Day[Int, Int]:

  type Model = Grid[Tile]

  override def parse(input: String): Model = Parser.parse(grid(_))(input).get

  override def part1(input: Model): Int = input.count {
    case (c, PaperRoll) => input.isAccessiblePaperRoll(c)
    case (_, Empty) => false
  }

  override def part2(input: Model): Int = part2(input, 0)

  @tailrec
  def part2(input: Model, step: Int): Int = {
    val r = part2step(input)
    r match
      case (_, 0) => step
      case (updatedModel, count)  => part2(updatedModel, step + count)
  }


  def part2step(input: Model): (Model, Int) = {
    val (count, tiles) = input.values.foldLeft((0, Seq.empty[(Coordinate, Tile)])) { case ((count, tiles), (c, t)) =>
      input.isAccessiblePaperRoll(c) match {
        case true =>
          (count + 1, tiles :+ (c, Empty))
        case false =>
          (count, tiles :+ (c, t))
      }
    }
    (Grid(tiles), count)
  }

  extension (g: Grid[Tile])
    def isAccessiblePaperRoll(c: Coordinate): Boolean = {
      g.values.get(c).contains(PaperRoll) &&
        Seq(
          c.transpose.leftUp,
          c.transpose.up,
          c.transpose.rightUp,
          c.transpose.left,
          c.transpose.right,
          c.transpose.leftDown,
          c.transpose.down,
          c.transpose.rightDown,
        )
          .flatMap {
            g.values.get
          }
          .count(_ == PaperRoll) < 4
    }

  enum Tile(symbol: Char):
    case Empty extends Tile('.')
    case PaperRoll extends Tile('@')

    override def toString: String = symbol.toString

  def tile[$: P]: P[(Int, Tile)] = P(Index ~ CharIn(".@").!).map {
    case (i , ".") => (i, Empty)
    case (i , "@") => (i, PaperRoll)
  }
  def grid[$: P]: P[Grid[Tile]] = P(TokenGrid.grid(tile))
