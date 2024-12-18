package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.TokenGrid.grid
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day8.Tile.Antenna
import net.hogerheijde.aoc2024.Day8.Tile.Space
import net.hogerheijde.aoc2024.Day8.Field
import net.hogerheijde.aoc2024.Day8.Frequency

import scala.jdk.CollectionConverters.*
import scala.util.Try

object Day8 extends Day[Int, Int]:

  type Model = Field

  override def parse(input: String): Model = Parser.parse(field(_))(input).get

  override def part1(input: Model): Int =
    input.availableFrequencies.flatMap { frequency =>
      input.antennasAt(frequency).map(_._1).antiNodes.filter(node => input.grid.inBounds(node))

    }.size

  override def part2(input: Model): Int =
    input.availableFrequencies.flatMap { frequency =>
      input.antinodessFor(input.antennasAt(frequency).map(_._1))
    }.size



  case class Field(grid: Grid[Tile]):
    def terse: Field = copy(Grid(grid.values.filterNot { case (_, t) => t == Space }))
    val availableFrequencies: Set[Frequency] = grid.values.filter(_._2 != Space).map(_._2.frequency).toSet
    def antennasAt(frequency: String): Set[(Coordinate, Antenna)] =
      grid.values.collect { case (c, t: Antenna) if t.frequency == frequency => (c, t) }.toSet

    def antinodessFor(nodes: Set[Coordinate]): Set[Coordinate] =
      nodes.subsets(2).flatMap(x => antinodesssssFor(x.head, x.last)).toSet

    def antinodesssssFor(node1: Coordinate, node2: Coordinate): Set[Coordinate] =
      val vector1 = node1 diff node2
      val vector2 = node2 diff node1

      val antinodes1 = Iterator.unfold(node1) { q =>
        val next = q add vector1
        if (grid.inBounds(next)) Some(next, next) else None
      }

      val antinodes2 = Iterator.unfold(node2) { q =>
        val next = q add vector2
        if (grid.inBounds(next)) Some(next, next) else None
      }

      antinodes1.toSet ++ antinodes2.toSeq

  sealed trait Tile:
    val frequency: Frequency
    override def toString: String = frequency

  type Frequency = String

  object Tile:
    case class Antenna(frequency: Frequency) extends Tile
    object Space extends Tile:
      override val frequency: String = "."

  extension (nodes: Set[Coordinate])
    def antiNodes: Set[Coordinate] = nodes.subsets(2).flatMap(x => (x.head, x.last).antiNodes).toSet

  extension (nodes: (Coordinate, Coordinate))
    def antiNodes: Set[Coordinate] =
      val antiNode1 = Coordinate(
        nodes._2.vertical + (nodes._2.vertical - nodes._1.vertical),
        nodes._2.horizontal + (nodes._2.horizontal - nodes._1.horizontal),
      )
      val antiNode2 = Coordinate(
        nodes._1.vertical + (nodes._1.vertical - nodes._2.vertical),
        nodes._1.horizontal + (nodes._1.horizontal - nodes._2.horizontal),
      )
      Set(antiNode1, antiNode2)

  def frequency[$: P]: P[(Int, Antenna)] = P(Index ~ CharIn("0-9a-zA-Z").!).map((i, f) => (i, Antenna(f)))
  def space[$: P]: P[(Int, Space.type)] = P(Index ~ ".").map((_ , Space))
  def tile[$: P]: P[(Int, Tile)] = P(space | frequency)
  def field[$: P]: P[Field] = P(grid(tile)).map(g => Field(g))