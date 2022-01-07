package net.hogerheijde.aoc2021

import fastparse.NoWhitespace._
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Line
import net.hogerheijde.aoc.common.parser.Common.coordinate
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day5 extends Day[Int, Int] {

  override type Model = Grid

  case class Grid(
    lines: Seq[Line]
  ) {
    val (minX, minY, maxX, maxY) = {
      lines match {
        case Seq() => (0, 0, 0, 0)
        case lines =>
          lines.foldLeft((0, 0, 9, 9)) {
            case ((minX, minY, maxX, maxY), nextLine) =>
              (
                Math.min(nextLine.minX, minX),
                Math.min(nextLine.minY, minY),
                Math.max(nextLine.maxX, maxX),
                Math.max(nextLine.maxY, maxY),
              )
          }
      }
    }

    val horivert: Grid = if (lines.exists { case Line(start, end) => start.x != end.x && start.y != end.y }) {
      Grid(lines.filter { case Line(start, end) => start.x == end.x || start.y == end.y })
    } else {
      this
    }

    def maximumOverlap(maxCount: Int): Int = {
      Range.inclusive(minY, maxY).flatMap { y =>
        Range.inclusive(minX, maxX).map { x =>
          lines.count(l => l.collision(Coordinate(x, y)))
        }
      }.count(_ >= maxCount)
    }

    override def toString: String = {
      val sb = new StringBuilder

      Range.inclusive(minY, maxY).foreach { y =>
        Range.inclusive(minX, maxX).foreach { x =>
          lines.count(l => l.collision(Coordinate(x, y))) match {
            case 0 => sb.append(".")
            case c => sb.append(c.toString)
          }
        }
        sb.append("\n")
      }
      sb.result()
    }
  }

  def grid[_: P]: P[Grid] = P(line.rep).map(Grid(_))
  def line[_: P]: P[Line] = P(coordinate ~ " -> " ~ coordinate ~ "\n").map { case (start, end) => Line(start, end)}

  override def parse(input: String): Model = Parser.parse(grid(_))(input).get

  override def part1(input: Model): Int = input.horivert.maximumOverlap(2)
  override def part2(input: Model): Int = input.maximumOverlap(2)
}
