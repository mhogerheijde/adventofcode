package net.hogerheijde.aoc2020

import fastparse.P
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.common.model.Coordinate2
import net.hogerheijde.aoc.grid.Grid
import net.hogerheijde.aoc.util.Day

sealed trait Tile
case object Floor extends Tile {
  override def toString: String = "."
}
case object EmptySeat extends Tile {
  override def toString: String = "L"
}
case object OccupiedSeat extends Tile {
  override def toString: String = "#"
}

object Day11 extends Day[Long, Long] {

  type Model = Grid[Tile]

  override def parse(input: String): Grid[Tile] = ???

  override def part1(input: Grid[Tile]): Long = ???

  override def part2(input: Grid[Tile]): Long = ???


  def floor[_: P]: P[Floor.type] = P(".").map(_ => Floor)
  def occupiedChair[_: P]: P[OccupiedSeat.type] = P("#").map(_ => OccupiedSeat)
  def emptyChair[_: P]: P[EmptySeat.type] = P("L").map(_ => EmptySeat)
  def tile[_: P]: P[Tile] = P(floor | occupiedChair | emptyChair)
  def tilesWithCoordinate[_: P](lineNo: Int): P[Seq[(Coordinate2, Tile)]] = P(tile.rep)
    .map(t => t.zipWithIndex.map { case (t, i) => (Coordinate2(i, lineNo), t) })

}
