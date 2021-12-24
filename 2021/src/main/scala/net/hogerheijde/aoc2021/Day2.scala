package net.hogerheijde.aoc2021

import fastparse.NoWhitespace._
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.common.parser.Common.number
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2021.Day2.Direction.Down
import net.hogerheijde.aoc2021.Day2.Direction.Forward
import net.hogerheijde.aoc2021.Day2.Direction.Up

object Day2 extends Day[Int, Int] {

  case class Location(depth: Int, horizontal: Int) {
    def go(dir: Direction): Location = dir match {
      case Up(amount) => Location(depth - amount, horizontal)
      case Down(amount) => Location(depth + amount, horizontal)
      case Forward(amount) => Location(depth, horizontal + amount)
    }
  }
  case class Location2(depth: Int, horizontal: Int, aim: Int) {
    def go(dir: Direction): Location2 = dir match {
      case Up(amount) => Location2(depth, horizontal, aim - amount)
      case Down(amount) => Location2(depth, horizontal, aim + amount)
      case Forward(amount) => Location2(depth + (amount * aim), horizontal + amount, aim)
    }
  }

  sealed trait Direction
  object Direction {
    case class Forward(amount: Int) extends Direction
    case class Up(amount: Int) extends Direction
    case class Down(amount: Int) extends Direction
  }

  type Model = Seq[Direction]

  def direction[_: P]: P[Direction] = P(up | down | forward)
  def up[_: P]: P[Up] = P("up " ~ number).map(Up(_))
  def down[_: P]: P[Down] = P("down " ~ number).map(Down(_))
  def forward[_: P]: P[Forward] = P("forward " ~ number).map(Forward(_))

  override def parse(input: String): Model = {
    Parser.standardLineSplit(input).map { line =>
      Parser.parse(direction(_))(line).get
    }
  }

  override def part1(input: Model): Int = {
    val endLocation = input.foldLeft(Location(0, 0)) { (current, direction) => current.go(direction) }
    endLocation.depth * endLocation.horizontal
  }

  override def part2(input: Model): Int = {
    val endLocation = input.foldLeft(Location2(0, 0, 0)) { (current, direction) => current.go(direction) }
    endLocation.depth * endLocation.horizontal
  }
}
