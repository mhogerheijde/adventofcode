package net.hogerheijde.aoc2022

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with Matchers {

  
  "Day 6" should {
    "solve part 1" in {
      Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz".toSeq) should be(5)
      Day6.part1("nppdvjthqldpwncqszvftbrmjlhg".toSeq) should be(6)
      Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toSeq) should be(10)
      Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toSeq) should be(11)

    }
    "solve part 2" in {
      Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb".toSeq) should be(19)
      Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz".toSeq) should be(23)
      Day6.part2("nppdvjthqldpwncqszvftbrmjlhg".toSeq) should be(23)
      Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".toSeq) should be(29)
      Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toSeq) should be(26)
    }
  }
}
