package net.hogerheijde.aoc2018.day9

import fastparse.NoWhitespace._
import fastparse.P
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018

object Day9 extends Day2018[Game, Int, Int]{
  override def name: String = "Day 9"


  override def parse(input: String): Game = {
    def wakesUp[_: P]: P[Game] = P(int ~" players; last marble is worth " ~ int ~ " points")
      .map { case (players, marbles) => Game.start(players, marbles) }
    Parser.parse(wakesUp(_))(input) match {
      case Some(g) => g
      case _ => throw new RuntimeException(s"Could not parse `$input` into a new Game.")
    }
  }

  override def part1(input: Game): Int = {
    val result = input.solve
    result.hiScore
  }

  override def part2(input: Game): Int = {
    val result = input.copy(noOfMarbles = input.noOfMarbles * 100).solve
    result.hiScore
  }
}
