package net.hogerheijde.aoc2019.day2

import net.hogerheijde.aoc2019.Day2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec




class Day2Test extends AnyWordSpec with Matchers {

  "Day2" should {
    "run example of part2" in {
      Day2.iterate(0, IndexedSeq(1,1,1,4,99,5,6,0,99)) should be (IndexedSeq(30,1,1,4,2,5,6,0,99))
      Day2.iterate(0, IndexedSeq(1,0,0,0,99)) should be (IndexedSeq(2,0,0,0,99))
      Day2.iterate(0, IndexedSeq(2,3,0,3,99)) should be (IndexedSeq(2,3,0,6,99))
      Day2.iterate(0, IndexedSeq(2,4,4,5,99,0)) should be (IndexedSeq(2,4,4,5,99,9801))
    }
  }
}
