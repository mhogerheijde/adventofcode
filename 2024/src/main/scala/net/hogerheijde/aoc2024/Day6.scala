package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Direction
import net.hogerheijde.aoc.common.model.Direction.East
import net.hogerheijde.aoc.common.model.Direction.North
import net.hogerheijde.aoc.common.model.Direction.NorthEast
import net.hogerheijde.aoc.common.model.Direction.NorthWest
import net.hogerheijde.aoc.common.model.Direction.South
import net.hogerheijde.aoc.common.model.Direction.SouthEast
import net.hogerheijde.aoc.common.model.Direction.SouthWest
import net.hogerheijde.aoc.common.model.Direction.West
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.common.parser.TokenGrid.IndexedToken
import net.hogerheijde.aoc.common.parser.TokenGrid.IndexedToken
import net.hogerheijde.aoc.common.parser.TokenGrid.grid
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day6.Guard
import net.hogerheijde.aoc2024.Day6.Tile.GuardTile
import net.hogerheijde.aoc2024.Day6.Tile.Obstacle
import net.hogerheijde.aoc2024.Day6.Tile.Path
import net.hogerheijde.aoc2024.Day6.Tile.Space

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.util.Try

object Day6 extends Day[Int, Int]:

  type Model = Field

  override def parse(input: String): Model = Parser.parse(field(_))(input).get

  override def part1(input: Model): Int =
    val solved = solvePt1(input)
    solved.pathCount

  @tailrec
  def solvePt1(field: Field): Field =
    if (field.isDone) field else solvePt1(field.step)

  override def part2(input: Model): Int =
    val originalSolution = solvePt1(input)
    val pathLocations: Seq[Coordinate] = originalSolution
      .grid.values
      .collect { case (x, y: Path.type) => (x, y) }
      .keys.toSeq

    pathLocations.zipWithIndex
      .par
      .map { case (nextLoc, i) =>
      val modifiedField = input.copy(grid = input.grid.copy(values = input.grid.values.updated(nextLoc, Obstacle)))
      val result = solvePt1(modifiedField).isLooping
//      if (result) System.out.print("x") else System.out.print(".")
//      if (i % 100 == 0) println()
//      System.out.flush()
      if (result) 1 else 0
    }.sum


  def tile[$: P]: P[IndexedToken[Tile]] = P((guard | path | space | obstacle))
  def obstacle[$: P]: P[IndexedToken[Obstacle.type]] = P(Index ~ "#").map((_, Obstacle))
  def guard[$: P]: P[IndexedToken[GuardTile]] = P(Index ~ CharIn("^v<>").!).map { case (i, d) =>
    val dir = d match
      case "^" => North
      case "v" => South
      case "<" => West
      case ">" => East
    (i, GuardTile(dir))
  }
  def path[$: P]: P[IndexedToken[Path.type]] = P(Index ~ "X").map((_, Path))
  def space[$: P]: P[IndexedToken[Space.type]] = P(Index ~ ".").map((_, Space))
  def field[$: P]: P[Field] = grid(tile).map { g =>
    val (initialGuardLocation: Coordinate, y: GuardTile) = g.values.collectFirst { case (c, g: GuardTile) => (c, g) }.get
    Field(
      Guard(y.direction, initialGuardLocation),
      Grid(g.values.filterNot(q => q._2.isInstanceOf[GuardTile])),
    )
  }

  case class Field(guard: Guard, grid: Grid[Tile], guardHistory: Set[Guard] = Set(), currentStep: Int = 0):
    val isOutOfBounds: Boolean = !grid.inBounds(guard.location)
    val isLooping: Boolean = guardHistory.contains(guard)
    val isDone: Boolean = isOutOfBounds || isLooping
    val pathCount: Int = grid.count { case (_, t) =>t.isInstanceOf[Path.type] }

    def terse: Field = copy(guard, Grid(grid.values.filterNot { case (_, t) => t == Space }))

    def pretty: String =
      if (isDone)
        Grid(grid.values).pretty + s"\nDone. (${if (grid.inBounds(guard.location)) "Loop" else "out-of-bounds" })"
      else
        Grid(grid.values + (guard.location -> GuardTile(guard.direction))).pretty

    def step(number: Int): Field = Range(0, number).foldLeft(this) { case (acc, _) => acc.step }
    def step: Field =
      if (isDone) return this

      val verticalObstacles = grid.values.toList.collect {
        case (c, _: Obstacle.type) if c.column == guard.location.column => c
      }
      val horizontalObstacles = grid.values.toList.collect {
        case (c, _: Obstacle.type) if c.row == guard.location.row => c
      }
      val (newGuardLocation, newPathTiles) = guard.direction match
        case North =>
          val newLocation = verticalObstacles.filter(_.row < guard.location.row)
            .maxByOption(_.row)
            .map(_.transpose.down)
            .getOrElse(Coordinate(grid.minRow - 1, guard.location.column))

          val pathsToAdd = Range(guard.location.row, newLocation.row, step = -1)
            .map(row => Coordinate(horizontal = guard.location.column, vertical = row))

          (newLocation, pathsToAdd)

        case South =>
          val newLocation = verticalObstacles.filter(_.row > guard.location.row)
            .minByOption(_.row)
            .map(_.transpose.up)
            .getOrElse(Coordinate(grid.maxRow + 1, guard.location.column))

          val pathsToAdd = Range(guard.location.row, newLocation.row)
            .map(row => Coordinate(horizontal = guard.location.column, vertical = row))

          (newLocation, pathsToAdd)
        case West =>
          val newLocation = horizontalObstacles.filter(_.column < guard.location.column)
            .maxByOption(_.column)
            .map(_.transpose.right)
            .getOrElse(Coordinate(guard.location.row, grid.minColumn - 1))

          val pathsToAdd = Range(guard.location.column, newLocation.column, step = -1)
            .map(col => Coordinate(horizontal = col, vertical = guard.location.row))

          (newLocation, pathsToAdd)

        case East =>
          val newLocation = horizontalObstacles.filter(_.column > guard.location.column)
            .minByOption(_.column)
            .map(_.transpose.left)
            .getOrElse(Coordinate(guard.location.row, grid.maxColumn + 1))

          val pathsToAdd = Range(guard.location.column, newLocation.column)
            .map(col => Coordinate(horizontal = col, vertical = guard.location.row))

          (newLocation, pathsToAdd)

        case d => throw IllegalStateException(s"Guard could not be looking $d")

      Field(
        guard.copy(location = newGuardLocation).turn,
        Grid(grid.values ++ newPathTiles.map(x => (x, Path))),
        guardHistory + guard,
        currentStep + 1,
      )

  sealed trait Tile

  object Tile:
    object Obstacle extends Tile:
      override def toString: String = "#"
    object Space extends Tile:
      override def toString: String = "."
    object Path extends Tile:
      override def toString: String = "X"
    case class GuardTile(direction: Direction) extends Tile:
      override def toString: String = direction match
        case North => "^"
        case South => "v"
        case East => ">"
        case West => "<"
        case d => throw IllegalStateException(s"Guard could not be looking $d")

  private val allowedDirections = Set(North, South, East, West)
  case class Guard(direction: Direction, location: Coordinate):
    assert(allowedDirections.contains(direction))
    def turn: Guard = direction match
      case North => Guard(East, location)
      case East => Guard(South, location)
      case South => Guard(West, location)
      case West => Guard(North, location)
      case d => throw IllegalStateException(s"Guard could not be looking $d")



