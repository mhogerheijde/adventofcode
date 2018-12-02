package net.hogerheijde.aco2018.day2

import net.hogerheijde.aoc2018.day2.Day2
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day2Test extends WordSpec with Matchers {

  val exampleInput = IndexedSeq(
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab",
  )


  "Day2" should {

    "create histogram" in {
      Day2.histogram("abcdefbdeded") should be (
        Map(
          'a' -> 1,
          'b' -> 2,
          'c' -> 1,
          'd' -> 4,
          'e' -> 3,
          'f' -> 1,
        )
      )
    }

    "count doubles" in {
      Day2.countDoubles(exampleInput) should be(4)
    }

     "count triplets" in {
      Day2.countTriplets(exampleInput) should be(3)
    }

    "calculate checksum" in {
      Day2.part1(exampleInput) should be (12)
    }
  }

}
