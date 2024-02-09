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
import net.hogerheijde.aoc2023.Day10.Tile.Ginside
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
import scala.collection.immutable
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


  override def part2(input: Model): Int =
    @tailrec
    def included(c: Coordinate, d: Direction = N, cnt: Set[Coordinate] = Set()): Set[Coordinate] =
      val (nextC, nextD) = next(input, c, d)
      input.values(nextC) match
        case Start => cnt + nextC
        case _ => included(nextC, nextD, cnt + nextC)

    val inLoop = included(start(input))
    val replaced: Map[Coordinate, Tile] = input.values.map {
      case (c, Start) => (c, connector(c, inLoop, input))
      case (c, t) if inLoop.contains(c) => (c, t)
      case (c, _) => (c, G)
    }

    println(Grid(replaced).pretty(_.display))

    val rows = replaced.maxBy { (c, _) => c.row }._1.row
    val columns = replaced.maxBy { (c, _) => c.column }._1.column

    val raytraced = scala.collection.mutable.Buffer.empty[(Coordinate, Tile)]
//    var outside = true
//    var outDirection = N
//    var prevTile = G
    (0 to rows).foreach { row =>
//      val count = scala.collection.mutable.Map(
//        Start -> 0,
//        G -> 0,
//        Ginside -> 0,
//        NS -> 0,
//        EW -> 0,
//        NE -> 0,
//        NW -> 0,
//        SW -> 0,
//        SE -> 0,
//      )
      var count = 0
//      var countVert = 0


      (0 to columns).foreach { col =>
        val c = Coordinate(row, col)
        val t = replaced(c)

        t match
          case G =>
//            if (count.exists { case (_, freq) => freq % 2 == 1} )
            if (count != 0)
              raytraced.append((c, Ginside))
            else
              raytraced.append((c, G))
          case Ginside => 0
          case EW =>
            raytraced.append((c, t))

          case NS =>
            if (count % 2  == 0) { count += 1} else { count -= 1}
            raytraced.append((c, t))

          case NW =>
            if (count % 2 != 0) count += 1
            raytraced.append((c, t))
          case SW =>
            if (count % 2 == 0) count += 1
            raytraced.append((c, t))
          case NE =>
            if (count % 2 != 0) count -= 1
            raytraced.append((c, t))
          case SE =>
            if (count % 2 == 0) count -= 1
            raytraced.append((c, t))

//        (outside, t) match {
//          // We should not have Start anymore, no Ginside should be outside, and G just stays G.
//          case (true, Start) | (true, Ginside) | (true, G) =>
//            raytraced = raytraced :+ (c, t)
//
////          case (true, NS) => raytraced = raytraced :+ (c, t); outside = false
////          case (false, NS) => raytraced = raytraced :+ (c, t); outside = true
//          case (_, NS) => raytraced = raytraced :+ (c, t); outside = !outside
//
//          // stays 'on the fence'
//          case (_, EW) => raytraced = raytraced :+ (c, t)
//          // stays 'on the fence'
////          case (false, EW) => raytraced = raytraced :+ (c, t)
//
//          // flips from out to inside
//          case (true, NE) => raytraced = raytraced :+ (c, t); outside = false
//          case (true, NW) => raytraced = raytraced :+ (c, t); outside = false
//          case (true, SE) => raytraced = raytraced :+ (c, t); outside = false
//
//
//          // we were outside, and we're moving left to right, so the fence moves away form us. Keep outisde
//          case (true, SW) => raytraced = raytraced :+ (c, t); outside = false
//
//          // if we're inside, and we meet a G, mark it as inside
//          case (false, G) => raytraced = raytraced :+ (c, Ginside)
//
//          case (false, Start) | (false, Ginside) =>
//            raytraced = raytraced :+ (c, t)
//
//          // flips inside to outside
//          case (false, NW) => raytraced = raytraced :+ (c, t) //; outside = true
//          case (false, SW) =>
//            raytraced = raytraced :+ (c, t)
//            if (outDirection == S) { outside = true }
//
//          case (false, SE) =>
//            raytraced = raytraced :+ (c, t) //; outside = true
//            if (outDirection == S) { outside = true }
//          case (false, NE) =>
//            raytraced = raytraced :+ (c, t)
//            if (outDirection == S) { outside = true } else { outDirection = S}
//
//        }
//        prevTile = t
      }
    }

    println(Grid(raytraced).pretty(_.display))

    0

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
      case G | Ginside => throw IllegalStateException("We should not be able to find ourselves at a ground tile")
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


  def connector(start: Coordinate, inLoop: Set[Coordinate], input: Model): Tile = {
    def connects(coordinate: Coordinate, tiles: Set[Tile]): Boolean =
      inLoop.contains(coordinate) && tiles.contains(input.values(coordinate))

    val connectsNorth: Boolean = connects(start.transpose.up, Set(NS, SE, SW))
    val connectsSouth: Boolean = connects(start.transpose.down, Set(NS, NE, NW))
    val connectsEast: Boolean = connects(start.transpose.right, Set(EW, NW, SW))
    val connectsWest: Boolean = connects(start.transpose.left, Set(EW, SE, NE))

    if (connectsNorth && connectsSouth) { NS }
    else if (connectsEast && connectsWest) { EW }
    else if (connectsNorth && connectsEast) { NE }
    else if (connectsNorth && connectsWest) { NW }
    else if (connectsSouth && connectsWest) { SW }
    else if (connectsSouth && connectsEast) { SE }
    else { Start }
  }


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
    case Ginside extends Tile("#", Set())
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