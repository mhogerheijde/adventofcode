package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.util.Day
import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.DigitGrid
import net.hogerheijde.aoc.util.Parser


object Day9 extends Day[Int, Int]{
  override type Model = Grid[Byte]

//  case class Grid(heightmap: Map[Coordinate, Byte])
//  case class Coordinate(x: Int, y: Int) {
//    def up = Coordinate(x, y - 1)
//    def down = Coordinate(x, y + 1)
//    def left = Coordinate(x - 1, y)
//    def right = Coordinate(x + 1, y)
//  }

  def depth[_: P]: P[(Int, Byte)] = P(Index ~ CharIn("0-9").!).map { case (column, char) => (column, char.toByte) }
  def row[_: P]: P[Seq[(Int, Byte)]] = P(depth.rep)
  def grid[_: P]: P[Grid[Byte]] = P((row ~ "\n").rep).map { rows =>
    Grid(rows.zipWithIndex.flatMap { case (depths, row) =>
      depths.map { case (column, depth) => Coordinate(column - (row * (depths.size + 1)), row) -> depth }
    }.toMap)
  }

  override def parse(input: String): Model = Parser.parse(DigitGrid.digitGrid(_))(input).get

  override def part1(input: Model): Int = {
    input.values.foldLeft(0) { case (result, (coordinate, depth)) =>
      val adjacent = Seq(
        input.values.get(coordinate.transpose.up),
        input.values.get(coordinate.transpose.down),
        input.values.get(coordinate.transpose.left),
        input.values.get(coordinate.transpose.right),
      ).flatten

      if (adjacent.exists { d => d < depth }) {
        result
      } else {
        result + depth + 1
      }
    }
  }

  override def part2(input: Model): Int = {
    val (lowPointLinkedList, lowpoints) = input.values
      .foldLeft(
        (Map.empty[Coordinate, Seq[Coordinate]], Seq.empty[Coordinate])
      ) { case ((basins, lowpoints), (coordinate, depth)) =>
        if (depth == 9) {
          (basins, lowpoints)
        } else {
          val adjacent = Seq(
            input.values.get(coordinate.transpose.up).map(coordinate.transpose.up -> _),
            input.values.get(coordinate.transpose.down).map(coordinate.transpose.down -> _),
            input.values.get(coordinate.transpose.left).map(coordinate.transpose.left -> _),
            input.values.get(coordinate.transpose.right).map(coordinate.transpose.right -> _),
          ).flatten

          adjacent.find { case (_, d) =>
              d < depth
          } match {
            case None =>
              (
                basins.updated(coordinate, basins.getOrElse(coordinate, Seq())),
                lowpoints :+ coordinate
              )
            case Some((lowerCoordinate, _)) =>
              (
                basins.updated(lowerCoordinate, basins.getOrElse(lowerCoordinate, Seq()) :+ coordinate),
                lowpoints
              )
          }
        }
      }

    def count(mapping: Map[Coordinate, Seq[Coordinate]])(c: Coordinate): Int = {
      mapping.get(c) match {
        case None => 0
        case Some(seq) => seq.map(count(mapping)(_)).sum + seq.size
      }
    }

    val sizes = lowpoints.map { p =>
      p -> (count(lowPointLinkedList)(p) + 1)
    }
    sizes.map(_._2).sorted.reverse.take(3).foldLeft(1) { case (total, next) => total * next}
  }
}
