package net.hogerheijde.aoc2018.day2

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

    "find candiate" in {
      Day2.candidate("abcde", "axcye") should be (false)
      Day2.candidate("fghij", "fguij") should be (true)
    }


    "solve part 2" in {
      Day2.part2(IndexedSeq(
        "abcde",
        "fghij",
        "klmno",
        "pqrst",
        "fguij",
        "axcye",
        "wvxyz",
      )) should be ("fgij")
    }


  }

}
