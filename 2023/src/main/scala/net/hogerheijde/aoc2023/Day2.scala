package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.util.Try

object Day2 extends Day[Int, Int]:

  type Model = Seq[Game]

  case class Game(
    number: Int,
    draws: Set[Draw],
  )

  case class Draw(
    red: Int,
    green: Int,
    blue: Int,
  ):
    val power = red * green * blue

  object Draw:
    def apply(colours: Seq[(Int, String)]): Draw =
      val red = colours.find(_._2 == "red").map(_._1).getOrElse(0)
      val green = colours.find(_._2 == "green").map(_._1).getOrElse(0)
      val blue = colours.find(_._2 == "blue").map(_._1).getOrElse(0)
      Draw(red, green, blue)


  override def parse(input: String): Model = Parser.parse(games)(input).get

  override def part1(input: Model): Int =
    val r: Draw => Boolean = (d: Draw) => d.red <= 12 && d.green <= 13 && d.blue <= 14
    input.filter(possible(_, r)).map(_.number).sum

  override def part2(input: Model): Int = input.map(g => minimalSet(g).power).sum


  def minimalSet(g: Game): Draw =
    g.draws.foldLeft(Draw(0, 0, 0)) { case (minimalDraw, nextDraw) =>
      minimalDraw.copy(
        Math.max(minimalDraw.red, nextDraw.red),
        Math.max(minimalDraw.green, nextDraw.green),
        Math.max(minimalDraw.blue, nextDraw.blue),
      )
    }

  def possible(g: Game, rule: Draw => Boolean): Boolean = g.draws.forall(rule)

  private[aoc2023] def games[$: P]: P[Seq[Game]] = P((game ~ "\n".?).rep)
  private[aoc2023] def game[$: P]: P[Game] =
    P("Game " ~ int ~ ": " ~ (draw ~ "; ".?).rep(min=1) ).map((x, y) => Game(x, y.toSet))
  private[aoc2023] def colour[$: P]: P[(Int, String)] = P(int ~ " " ~ ("red".! | "green".! | "blue".!))
  private[aoc2023] def draw[$: P]: P[Draw] = P((colour ~ ", ".?).rep(min=1, max=3)).map(Draw(_))
