package net.hogerheijde.aoc2019.day1

import org.scalatest.Matchers
import org.scalatest.WordSpec



class Day1Test extends WordSpec with Matchers {


  "Day1" should {


    "run example of part2" in {
      Day1.part2(Day1.parse("1969")) should be (966)
      Day1.part2(Day1.parse("100756")) should be (50346)
      Day1.part2(Day1.parse("-6\n+3\n+8\n+5\n-6")) should be(5)
//      Day1.part2(Day1.parse("+7\n+7\n-2\n-7\n-4")) should be (14)
    }

  }
}
