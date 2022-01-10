package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.util.Day
import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.util.Parser


object Day9 extends Day[Int, Int]{
  override type Model = Grid

  case class Grid(heightmap: Map[Coordinate, Short])
  case class Coordinate(row: Int, column: Int) {
    def up = Coordinate(row - 1, column)
    def down = Coordinate(row + 1, column)
    def left = Coordinate(row, column - 1)
    def right = Coordinate(row, column + 1)
  }

  def depth[_: P]: P[(Int, Short)] = P(Index ~ CharIn("0-9").!).map { case (column, char) => (column, char.toShort) }
  def row[_: P]: P[Seq[(Int, Short)]] = P(depth.rep)
  def grid[_: P]: P[Grid] = P((row ~ "\n").rep).map { rows =>
    Grid(rows.zipWithIndex.flatMap { case (depths, row) =>
      depths.map { case (column, depth) => Coordinate(row, column - (row * (depths.size + 1))) -> depth }
    }.toMap)
  }

  override def parse(input: String): Grid = Parser.parse(grid(_))(input).get

  override def part1(input: Grid): Int = {
    input.heightmap.foldLeft(0) { case (result, (coordinate, depth)) =>
      val adjacent = Seq(
        input.heightmap.get(coordinate.up),
        input.heightmap.get(coordinate.down),
        input.heightmap.get(coordinate.left),
        input.heightmap.get(coordinate.right),
      ).flatten

      if (adjacent.exists { d => d < depth }) {
        result
      } else {
        result + depth + 1
      }
    }
  }

  override def part2(input: Grid): Int = {
    val (lowPointLinkedList, lowpoints) = input.heightmap
      .foldLeft(
        (Map.empty[Coordinate, Seq[Coordinate]], Seq.empty[Coordinate])
      ) { case ((basins, lowpoints), (coordinate, depth)) =>
        if (depth == 9) {
          (basins, lowpoints)
        } else {
          val adjacent = Seq(
            input.heightmap.get(coordinate.up).map(coordinate.up -> _),
            input.heightmap.get(coordinate.down).map(coordinate.down -> _),
            input.heightmap.get(coordinate.left).map(coordinate.left -> _),
            input.heightmap.get(coordinate.right).map(coordinate.right -> _),
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
