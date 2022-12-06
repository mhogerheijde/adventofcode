package net.hogerheijde.aoc2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  
  "Day 6" should {
    "solve part 1" in {
      Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz") should be(5)
      Day6.part1("nppdvjthqldpwncqszvftbrmjlhg") should be(6)
      Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(10)
      Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") should be(11)

    }
    "solve part 2" in {
      Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") should be(19)
      Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz") should be(23)
      Day6.part2("nppdvjthqldpwncqszvftbrmjlhg") should be(23)
      Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(29)
      Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") should be(26)
    }
  }
}
