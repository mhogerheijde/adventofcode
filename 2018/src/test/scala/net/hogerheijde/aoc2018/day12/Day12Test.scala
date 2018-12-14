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
      Farm.parse(example) should be  (
        Farm(
          expectedState,
          expectedRules
        )
      )


    }

    "calculate next state" in {
      exampleFarm.next.state.toString should be ("....#...#....#.....#..#..#..#....")
    }

  }

}
