package net.hogerheijde.aoc2018.day10

import scala.annotation.tailrec

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.parser.Common.coordinate
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018

case class Velocity(x: Int, y: Int)
case class Point(coordinate: Coordinate, velocity: Velocity) {
  def advance: Point = this.copy(coordinate = Coordinate(coordinate.x + velocity.x, coordinate.y + velocity.y))
}

case class Field(pointsByVelocity: Map[Velocity, IndexedSeq[Point]]) {

  val points = pointsByVelocity.values.flatten
  val hiPoint: Int =     points.minBy(_.coordinate.y).coordinate.y
  val lowPoint: Int =    points.maxBy(_.coordinate.y).coordinate.y
  def leftPoint: Int  =  points.minBy(_.coordinate.x).coordinate.x
  def rightPoint: Int  = points.maxBy(_.coordinate.x).coordinate.x


  val height: Int = lowPoint - hiPoint

  def advance: Field = { Field(pointsByVelocity.map(ps => (ps._1, ps._2.map(_.advance)))) }

  def pretty: String = {
    val b = new StringBuilder()
    Range.inclusive(hiPoint, lowPoint) foreach { y =>
      Range.inclusive(leftPoint, rightPoint) foreach { x =>
        val char = if (points.exists(p => p.coordinate == Coordinate(x, y))) {"#"} else {"."}
        b append char
      }
      b append "\n"
    }
    b.toString.trim
  }
}
object Field {

  private def point[_: P]: P[Point] =
      P("position=<" ~ " ".rep.? ~ coordinate ~ "> velocity=<" ~ " ".rep.? ~ coordinate ~">")
        .map { case (c1, c2) => Point(c1, Velocity(c2.x, c2.y))}

  def parse(input: String): Field = {
    Field(input.trim.lines.flatMap(line =>
      Parser.parse(point(_))(line)
    ).toIndexedSeq.groupBy(p => p.velocity))
  }
}

object Day10 extends Day2018[(Field, Int), String, Int]{
  override def name: String = "Day 10"

  override def parse(input: String): (Field, Int) = {
    val initialField = Field.parse(input)
    solve(initialField)
  }

  def solve(f: Field): (Field, Int) = {
    solve(f.advance, f, 1)
  }

  @tailrec
  private def solve(f: Field, previous: Field, i: Int): (Field, Int) = {
    val advance = f.advance
    if (f.height < advance.height) { (f, i) } else { solve(advance, f, i + 1) }
  }

  override def part1(input: (Field, Int)): String = {
    "\n" + input._1.pretty
  }

  override def part2(input: (Field, Int)): Int = {
    input._2
  }
}
