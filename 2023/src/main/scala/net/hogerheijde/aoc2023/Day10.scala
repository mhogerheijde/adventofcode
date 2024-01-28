package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day10.Direction.E
import net.hogerheijde.aoc2023.Day10.Direction.N
import net.hogerheijde.aoc2023.Day10.Direction.S
import net.hogerheijde.aoc2023.Day10.Direction.W
import net.hogerheijde.aoc2023.Day10.Tile.EW
import net.hogerheijde.aoc2023.Day10.Tile.G
import net.hogerheijde.aoc2023.Day10.Tile.NE
import net.hogerheijde.aoc2023.Day10.Tile.NS
import net.hogerheijde.aoc2023.Day10.Tile.NS
import net.hogerheijde.aoc2023.Day10.Tile.NW
import net.hogerheijde.aoc2023.Day10.Tile.NW
import net.hogerheijde.aoc2023.Day10.Tile.SE
import net.hogerheijde.aoc2023.Day10.Tile.SE
import net.hogerheijde.aoc2023.Day10.Tile.SW
import net.hogerheijde.aoc2023.Day10.Tile.SW
import net.hogerheijde.aoc2023.Day10.Tile.Start

import scala.annotation.tailrec
import scala.util.Try

object Day10 extends Day[Int, Int]:

  type Model = Grid[Tile]

  override def parse(input: String): Model = Parser.parse(grid(_))(input).get

  override def part1(input: Model): Int =
    @tailrec
    def count(c: Coordinate, d: Direction = N, cnt: Int = 0): Int =
      val (nextC, nextD) = next(input, c, d)
      input.values(nextC) match
        case Start => cnt
        case _ => count(nextC, nextD, cnt + 1)

    Math.ceil(count(start(input)) / 2.0).toInt


  override def part2(input: Model): Int = 0

  def start(input: Model): Coordinate = input.values.find((_, t) => t == Start).get._1

  extension (c: Coordinate)
    def go(d: Direction): Coordinate =
      d match
        case N => c.transpose.up
        case S => c.transpose.down
        case E => c.transpose.right
        case W => c.transpose.left

  def next(input: Model, coordinate: Coordinate, from: Direction): (Coordinate, Direction) =
    val newDirection: Direction = input.values(coordinate) match
      case G => throw IllegalStateException("We should not be able to find ourselves at a ground tile")
      case Start =>
        // Search from north, clockwise for first connecting pipe
        Seq(N, E, S, W) .find(d =>
          val newC = coordinate.go(d)
          val newTile = input.values(newC)
          newTile.connects.contains(d.flip)
        ).get
      case EW | NE | NS | NW | SE | SW =>
        (input.values(coordinate).connects - from).head

    (coordinate.go(newDirection), newDirection.flip)


  def tile[$: P]: P[Tile] = P(CharIn(".|SFJL7\\-").!).map(t => Tile.fromString(t).get)
  def line[$: P]: P[Seq[(Int, Tile)]] = P(Index ~ (Index ~ tile).rep).
    map((startOfLine, tiles) => tiles.map((i, t) => i - startOfLine -> t))

  def grid[$: P]: P[Grid[Tile]] = P((line ~ "\n").rep ~ line)
    .map(_ :+ _)
    .map(lines =>
      // Seq of lines, where lines are Seq(indexOfTile, tile)
      lines.zipWithIndex.foldLeft(Grid.empty[Tile]) { case (grid, (row, rowNumber)) =>
        row.foldLeft(grid) { case (subGrid, (column, tile)) =>
          subGrid.add(Coordinate(vertical = rowNumber, horizontal = column), tile)
        }
      }
    )

  enum Direction:
    case N, S, E, W
    def flip: Direction =
      this match
        case N => S
        case S => N
        case E => W
        case W => E

  enum Tile(val display: String, val connects: Set[Direction]):
    case Start extends Tile("S", Set(N, S, E, W))
    case G extends Tile(" ", Set())
    case NS extends Tile("│", Set(N, S))
    case EW extends Tile("─", Set(E, W))
    case NE extends Tile("└", Set(N, E))
    case NW extends Tile("┘", Set(N, W))
    case SW extends Tile("┐", Set(S, W))
    case SE extends Tile("┌", Set(S, E))
  object Tile:
    def fromString(s: String): Option[Tile] =
      s match
        case "S" => Some(Start)
        case "." => Some(G)
        case "|" => Some(NS)
        case "-" => Some(EW)
        case "L" => Some(NE)
        case "J" => Some(NW)
        case "7" => Some(SW)
        case "F" => Some(SE)
        case _ => None