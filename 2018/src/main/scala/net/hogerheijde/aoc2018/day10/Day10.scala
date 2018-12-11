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

case class Field(points: IndexedSeq[Point]) {

  val hiPoint: Int =     points.minBy(_.coordinate.y).coordinate.y
  val lowPoint: Int =    points.maxBy(_.coordinate.y).coordinate.y
  def leftPoint: Int  =  points.minBy(_.coordinate.x).coordinate.x
  def rightPoint: Int  = points.maxBy(_.coordinate.x).coordinate.x


  val showsMessage: Boolean = lowPoint - hiPoint == 7

  def advance: Field = { Field(points.map(_.advance)) }

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
    ).toIndexedSeq)
  }
}

object Day10 extends Day2018[Field, String, String]{
  override def name: String = "Day 10"

  override def parse(input: String): Field = {
    Field.parse(input)
  }

  override def part1(input: Field): String = {

    @tailrec
    def solve(f: Field, i: Int = 0): Field = {
//      if (i%100==0) {print(".")}
      if (i%10000==0) {println(f.pretty + "\n\n\n") }

      if (f.showsMessage) { f } else { solve(f.advance, i + 1) }
    }

    val solved = solve(input)

    solved.pretty

  }

  override def part2(input: Field): String = ???
}
