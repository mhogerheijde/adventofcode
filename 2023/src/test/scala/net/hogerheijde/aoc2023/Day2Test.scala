package net.hogerheijde.aoc2023

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*
import net.hogerheijde.aoc2023.Day2.Draw
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day2.Game

class Day2Test extends AnyWordSpec with Matchers:

  val exampleInput: String =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin


  "Day 2 parser" should {
    "parse draw" in {
      Parser.parse(Day2.colour(_))("1 red") should be (Some((1, "red")))
      Parser.parse(Day2.colour(_))("2 green") should be (Some((2, "green")))
      Parser.parse(Day2.colour(_))("3 blue") should be (Some((3, "blue")))

      Parser.parse(Day2.draw(_))("1 red, 2 green, 3 blue") should be (Some(Draw(1, 2, 3)))

      Parser.parse(Day2.draw(_))("2 green, 3 blue") should be (Some(Draw(0, 2, 3)))
      Parser.parse(Day2.draw(_))("1 red, 3 blue") should be (Some(Draw(1, 0, 3)))
      Parser.parse(Day2.draw(_))("1 red, 2 green") should be (Some(Draw(1, 2, 0)))

      Parser.parse(Day2.draw(_))("1 red, 3 blue, 2 green") should be(Some(Draw(1, 2, 3)))
      Parser.parse(Day2.draw(_))("3 blue, 1 red, 2 green") should be(Some(Draw(1, 2, 3)))
      Parser.parse(Day2.draw(_))("2 green, 3 blue, 1 red") should be(Some(Draw(1, 2, 3)))
    }
  }

  "Day 2" should {

    "parse input" in {
      Day2.parse(exampleInput) should be(
        Seq(
          Game(1, Set(Draw(4, 0, 3), Draw(1, 2, 6), Draw(0, 2, 0))),
          Game(2, Set(Draw(0, 2, 1), Draw(1, 3, 4), Draw(0, 1, 1))),
          Game(3, Set(Draw(20, 8, 6), Draw(4, 13, 5), Draw(1, 5, 0))),
          Game(4, Set(Draw(3, 1, 6), Draw(6, 3, 0), Draw(14, 3, 15))),
          Game(5, Set(Draw(6, 3, 1), Draw(1, 2, 2))),
        )
      )
    }

    "Part1: example answer" in {
      Day2.part1(Day2.parse(exampleInput)) should be(8)
    }

    "Part2: example answer" in {
      Day2.part2(Day2.parse(exampleInput)) should be(2286)
    }
  }