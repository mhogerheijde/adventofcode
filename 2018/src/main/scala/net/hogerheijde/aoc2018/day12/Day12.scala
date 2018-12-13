package net.hogerheijde.aoc2018.day12

import fastparse.P
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day10.Field
import net.hogerheijde.aoc2018.day10.Point
import net.hogerheijde.aoc2018.day10.Velocity

object Day12 extends Day2018[Plants, Int, String]{
  override def name: String = "Day 12"

  override def parse(input: String): Plants = {

  }

  override def part1(input: Plants): Int = ???

  override def part2(input: Plants): String = ???
}

case class Plants()


object Plants {


  private def initialState[_: P]: P[State] =
      P("position=<" ~ " ".rep.? ~ coordinate ~ "> velocity=<" ~ " ".rep.? ~ coordinate ~">")
        .map { case (c1, c2) => Point(c1, Velocity(c2.x, c2.y))}

  def parse(input: String): Field = {
    Field(input.trim.lines.flatMap(line =>
      Parser.parse(point(_))(line)
    ).toIndexedSeq.groupBy(p => p.velocity))
  }
}

sealed trait Space
case object Plant extends Space
case object Pot extends Space

case class State(pots: Map[Int, Space])
