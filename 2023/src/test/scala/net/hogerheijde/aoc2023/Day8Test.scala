package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.CircularBuffer
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day8.Direction.L
import net.hogerheijde.aoc2023.Day8.Direction.R
import net.hogerheijde.aoc2023.Day8.game
import net.hogerheijde.aoc2023.Day8.lcm
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day8Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)
      |""".stripMargin

  val exampleInput2: String =
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)
      |""".stripMargin



  val exampleModel: Day8.Model =
    (
      CircularBuffer(L, L, R),
      Day8.Graph(Map(
        "AAA" -> ("BBB", "BBB"),
        "BBB" -> ("AAA", "ZZZ"),
        "ZZZ" -> ("ZZZ", "ZZZ"),
      )),
    )

  val exampleModel2: Day8.Model = Day8.parse(exampleInput2)


  "Day 8 parser" should {
    "parse line" in {
      Parser.parse(Day8.nodeLine(_))("AAA = (BBB, BBB)").get should be (("AAA", "BBB", "BBB"))
    }
    "graph" in {
      val graph =
        """AAA = (BBB, BBB)
          |BBB = (AAA, ZZZ)
          |ZZZ = (ZZZ, ZZZ)""".stripMargin
      Parser.parse(Day8.graph(_))(graph).get should be(
        Day8.Graph(Map(
          "AAA" -> ("BBB", "BBB"),
          "BBB" -> ("AAA", "ZZZ"),
          "ZZZ" -> ("ZZZ", "ZZZ"),
        ))
      )
    }
  }

  "Seq" should {
    "find lcm" in {
      Seq(7, 2).lcm should be (14)
      Seq(8, 1).lcm should be (8)
      Seq(6, 4, 8).lcm should be (24)
      Seq(8, 2, 1, 10).lcm should be (40)
      Seq(9, 6, 2, 1, 5).lcm should be (90)
      Seq(5, 5, 7, 1, 1).lcm should be (35)
      Seq(4, 13, 8, 8, 11, 1).lcm should be (1144)
      Seq(7, 2, 2, 11, 11, 8, 5).lcm should be (3080)
      Seq(1, 6, 10, 3, 4, 10, 7).lcm should be (420)
      Seq(5, 2, 9, 10, 3, 4, 4, 4, 7).lcm should be (1260)
      Seq(9, 7, 10, 9, 7, 8, 5, 10, 1).lcm should be (2520)
    }
  }

  "Day 8" should {

    "find day 2 startnodes" in {
      Day8.startNodes(exampleModel2._2) should be (Set("11A", "22A"))
    }

    "parse input" in {
      Day8.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day8.part1(Day8.parse(exampleInput)) should be(6)
    }

    "Part2: example answer" in {
      Day8.part2(Day8.parse(exampleInput2)) should be(6)
    }
  }
}