package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4
      |""".stripMargin


  val exampleSeeds: Day5.Rules = Day5.Rules(
    Seq(79, 14, 55, 13),
    Map(
      "seed" -> Day5.Mapping(
        "seed",
        "soil",
        Seq(
          Day5.Range(98, 50, 2),
          Day5.Range(50, 52, 48),
        )
      ),
      "soil" -> Day5.Mapping(
        "soil",
        "fertilizer",
        Seq(
          Day5.Range(15, 0, 37),
          Day5.Range(52, 37, 2),
          Day5.Range(0, 39, 15),
        )
      ),
      "fertilizer" -> Day5.Mapping(
        "fertilizer",
        "water",
        Seq(
          Day5.Range(53, 49, 8),
          Day5.Range(11, 0, 42),
          Day5.Range(0, 42, 7),
          Day5.Range(7, 57, 4),
        )
      ),
      "water" -> Day5.Mapping(
        "water",
        "light",
        Seq(
          Day5.Range(18, 88, 7),
          Day5.Range(25, 18, 70),
        )
      ),
      "light" -> Day5.Mapping(
        "light",
        "temperature",
        Seq(
          Day5.Range(77, 45, 23),
          Day5.Range(45, 81, 19),
          Day5.Range(64, 68, 13),
        )
      ),
      "temperature" -> Day5.Mapping(
        "temperature",
        "humidity",
        Seq(
          Day5.Range(69, 0, 1),
          Day5.Range(0, 1, 69),
        )
      ),
      "humidity" -> Day5.Mapping(
        "humidity",
        "location",
        Seq(
          Day5.Range(56, 60, 37),
          Day5.Range(93, 56, 4),
        )
      ),
    )
  )

  "Range" should {
    "apply mapping" in {
      val exampleRange1 = Day5.Range(98, 50, 2)
      exampleRange1.map(97) should be (None)
      exampleRange1.map(98) should be (Some(50))
      exampleRange1.map(99) should be (Some(51))
      exampleRange1.map(100) should be (None)

      val exampleRange2 = Day5.Range(50, 52, 48)

      exampleRange2.map(49) should be (None)
      Range(50, 98).foreach { i =>
        exampleRange2.map(i) should be (Some(i + 2))
      }
      exampleRange2.map(98) should be (None)

    }
  }

  "Mapping" should {
    "map values" in {
      val m = Day5.Mapping("seed", "soil", Seq(
        Day5.Range(98, 50, 2),
        Day5.Range(50, 52, 48),
      ))

      Seq(
          0 -> 0,
          1 -> 1,
          48 -> 48,
          49 -> 49,
          50 -> 52,
          51 -> 53,
          96 -> 98,
          97 -> 99,
          98 -> 50,
          99 -> 51,
      ).foreach { (input, expectation) =>
        m.map(input) should be (expectation)
      }

    }
  }

  "Day 5" should {
    "parse input" in {
      Day5.parse(exampleInput) should be(exampleSeeds)
    }

    "find next ingredient" in {
      Day5.find(exampleSeeds.mappings)(79, "seed") should be ((81, "soil"))
      Day5.find(exampleSeeds.mappings)(14, "seed") should be ((14, "soil"))
      Day5.find(exampleSeeds.mappings)(55, "seed") should be ((57, "soil"))
      Day5.find(exampleSeeds.mappings)(13, "seed") should be ((13, "soil"))

      // Manually check all chain in example 1
      Day5.find(exampleSeeds.mappings)(79, "seed") should be((81, "soil"))
      Day5.find(exampleSeeds.mappings)((81, "soil")) should be((81, "fertilizer"))
      Day5.find(exampleSeeds.mappings)((81, "fertilizer")) should be((81, "water"))
      Day5.find(exampleSeeds.mappings)((81, "water")) should be((74, "light"))
      Day5.find(exampleSeeds.mappings)((74, "light")) should be((78, "temperature"))
      Day5.find(exampleSeeds.mappings)((78, "temperature")) should be((78, "humidity"))
      Day5.find(exampleSeeds.mappings)((78, "humidity")) should be((82, "location"))
    }

    "resolve chain" in {
      Day5.resolve(exampleSeeds.mappings, "location")((79, "seed")) should be ((82, "location"))
    }

    "Part1: example answer" in {
      Day5.part1(Day5.parse(exampleInput)) should be(35)
    }

    "Part2: example answer" in {
      Day5.part2(Day5.parse(exampleInput)) should be(46)
    }
  }
}