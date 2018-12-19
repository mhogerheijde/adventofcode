package net.hogerheijde.aoc2018.day12

import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day12Test extends WordSpec with Matchers {


  val exampleFarm = Farm.parse(
        """initial state: #..#.#..##......###...###
          |
          |...## => #
          |..#.. => #
          |.#... => #
          |.#.#. => #
          |.#.## => #
          |.##.. => #
          |.#### => #
          |#.#.# => #
          |#.### => #
          |##.#. => #
          |##.## => #
          |###.. => #
          |###.# => #
          |####. => #
          |""".stripMargin)

  "Farm" should {
    "parse example" in {


      val example =
        """initial state: #..#.#..##......###...###
          |
          |...## => #
          |..#.. => #
          |.#... => #
          |.#.#. => #
          |.#.## => #
          |.##.. => #
          |.#### => #
          |#.#.# => #
          |#.### => #
          |##.#. => #
          |##.## => #
          |###.. => #
          |###.# => #
          |####. => #
          |""".stripMargin

      val expectedState = State(IndexedSeq(
        Plant, Pot, Pot, Plant, Pot, Plant, Pot, Pot, Plant,
        Plant, Pot, Pot, Pot, Pot, Pot, Pot, Plant, Plant, Plant,
        Pot, Pot, Pot, Plant, Plant, Plant))

      val expectedRules = IndexedSeq(
        Rule(Pattern(Pot, Pot, Pot, Plant, Plant), Plant),
        Rule(Pattern(Pot, Pot, Plant, Pot, Pot), Plant),
        Rule(Pattern(Pot, Plant, Pot, Pot, Pot), Plant),
        Rule(Pattern(Pot, Plant, Pot, Plant, Pot), Plant),
        Rule(Pattern(Pot, Plant, Pot, Plant, Plant), Plant),
        Rule(Pattern(Pot, Plant, Plant, Pot, Pot), Plant),
        Rule(Pattern(Pot, Plant, Plant, Plant, Plant), Plant),
        Rule(Pattern(Plant, Pot, Plant, Pot, Plant), Plant),
        Rule(Pattern(Plant, Pot, Plant, Plant, Plant), Plant),
        Rule(Pattern(Plant, Plant, Pot, Plant, Pot), Plant),
        Rule(Pattern(Plant, Plant, Pot, Plant, Plant), Plant),
        Rule(Pattern(Plant, Plant, Plant, Pot, Pot), Plant),
        Rule(Pattern(Plant, Plant, Plant, Pot, Plant), Plant),
        Rule(Pattern(Plant, Plant, Plant, Plant, Pot), Plant),
      )


      val resultFarm = Farm.parse(example)
      resultFarm should be  (
        Farm(
          expectedState,
          expectedRules
        )
      )


    }

    "calculate next state" in {
      val gen1 = exampleFarm.next
      val gen2 = gen1.next
      val gen3 = gen2.next
      gen1.state.toString should be ("-5.....#...#....#.....#..#..#..#.....")
      gen2.state.toString should be ("-5.....##..##...##....#..#..#..##.....")
      gen3.state.toString should be ("-6.....#.#...#..#.#....#..#..#...#.....")

      val gen20 = exampleFarm.next(20)
      gen20.state.toString should be ("-7.....#....##....#####...#######....#.#..##.....")
    }

    "calculate plant value" in {
      exampleFarm.next(20).value should be (325)
    }

  }

}
