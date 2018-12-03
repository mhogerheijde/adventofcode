package net.hogerheijde.aoc2018.day1

import net.hogerheijde.aoc2018.day1.day1.Drift
import net.hogerheijde.aoc2018.day1.day1.Minus
import net.hogerheijde.aoc2018.day1.day1.Plus
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day1Test extends WordSpec with Matchers {


  "Day1" should {
    "parse input" in {
      Day1.parse("+1\n+25\n-397") should be (IndexedSeq(
        Drift(Plus, 1),
        Drift(Plus, 25),
        Drift(Minus, 397),
      ))
    }

    "run example of part1" in {
      Day1.part1(Day1.parse("+1\n+1\n+1")) should be (3)
      Day1.part1(Day1.parse("+1\n+1\n-2")) should be (0)
      Day1.part1(Day1.parse("-1\n-2\n-3")) should be (-6)
    }

    "run example of part2" in {
      Day1.part2(Day1.parse("+1\n-1")) should be (0)
      Day1.part2(Day1.parse("+3\n+3\n+4\n-2\n-4 ")) should be (10)
      Day1.part2(Day1.parse("-6\n+3\n+8\n+5\n-6")) should be(5)
      Day1.part2(Day1.parse("+7\n+7\n-2\n-7\n-4")) should be (14)
    }

  }
}
