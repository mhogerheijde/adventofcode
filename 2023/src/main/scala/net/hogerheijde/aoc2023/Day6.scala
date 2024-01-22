package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.long
import net.hogerheijde.aoc.common.parser.Common.uint
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.language.implicitConversions
import scala.util.Try
import scala.util.chaining.*

object Day6 extends Day[Int, Int]:

  type Model = Seq[Race]

  override def parse(input: String): Model = Parser.parse(races(_))(input).get

  override def part1(input: Model): Int = input.map(_.betterOptions.size).product

  override def part2(input: Model): Int =
    input
      .foldLeft(("", "")) { (result, nextRace) =>
        (result._1 + nextRace.time, result._2 + nextRace.record)
      }
      .pipe((t, d) => Race(t.toInt, d.toLong))
      .betterOptions.size

  case class Play(
    press: Int,
    resultDistance: Long,
  )

  case class Race(time: Int, record: Long):
    def options: Seq[Play] = (0 to time).map { press => Play(press, press.toLong * (time - press)) }
    def betterOptions: Seq[Play] = options.filter(_.resultDistance > record)

  def races[$: P]: P[Seq[Race]] = P(times ~ distances).map((ts, ds) => ts.zip(ds).map(Race.apply))
  private def times[$: P]: P[Seq[Int]] = P("Time:" ~ (" ".rep(min=1) ~ uint).rep(min = 1) ~ "\n")
  private def distances[$: P]: P[Seq[Long]] = P("Distance:" ~ (" ".rep(min=1) ~ long).rep(min = 1))
