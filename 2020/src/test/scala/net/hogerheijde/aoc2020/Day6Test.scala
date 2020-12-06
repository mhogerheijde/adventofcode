package net.hogerheijde.aoc2020

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers{

  val exampleInput =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  "Day6" should {
    "parse" in {
      Day6.parse(exampleInput) should be (Seq(
        Seq(Set('a', 'b', 'c')),
        Seq(Set('a'), Set('b'), Set('c')),
        Seq(Set('a', 'b'), Set('a', 'c')),
        Seq(Set('a'), Set('a'), Set('a'), Set('a')),
        Seq(Set('b')),
      ))
    }

    "calculate part 1" in  {
      Day6.part1(Day6.parse(exampleInput)) should be (11)
    }

    "calculate part 2" in  {
      Day6.part2(Day6.parse(exampleInput)) should be (6)
    }

  }


}
