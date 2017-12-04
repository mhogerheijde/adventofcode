package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day3.Day3
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day3Test extends WordSpec with Matchers {

  /*
  Example grid

37  36  35  34  33  32  31
38  17  16  15  14  13  30
39  18   5   4   3  12  29
40  19   6   1   2  11  28
41  20   7   8   9  10  27
42  21  22  23  24  25  26
43  44  45  46  47  48  49
   */


  "Day3" should {
    "calculate ring" in {
      Day3.ringOf(1) should be(0)

      Day3.ringOf(2) should be(1)
      Day3.ringOf(3) should be(1)
      Day3.ringOf(4) should be(1)
      Day3.ringOf(5) should be(1)
      Day3.ringOf(6) should be(1)
      Day3.ringOf(7) should be(1)
      Day3.ringOf(8) should be(1)
      Day3.ringOf(9) should be(1)

      Day3.ringOf(10) should be(2)
      Day3.ringOf(11) should be(2)
      Day3.ringOf(12) should be(2)
      Day3.ringOf(13) should be(2)
      Day3.ringOf(14) should be(2)
      Day3.ringOf(15) should be(2)
      Day3.ringOf(16) should be(2)
      Day3.ringOf(17) should be(2)
      Day3.ringOf(18) should be(2)
      Day3.ringOf(19) should be(2)
      Day3.ringOf(20) should be(2)
      Day3.ringOf(21) should be(2)
      Day3.ringOf(22) should be(2)
      Day3.ringOf(23) should be(2)
      Day3.ringOf(24) should be(2)
      Day3.ringOf(25) should be(2)

      Day3.ringOf(26) should be(3)
      Day3.ringOf(27) should be(3)
      Day3.ringOf(28) should be(3)
      Day3.ringOf(29) should be(3)
      Day3.ringOf(30) should be(3)
      Day3.ringOf(31) should be(3)
      Day3.ringOf(32) should be(3)
      Day3.ringOf(33) should be(3)
      Day3.ringOf(34) should be(3)
      Day3.ringOf(35) should be(3)
      Day3.ringOf(36) should be(3)
      Day3.ringOf(37) should be(3)
      Day3.ringOf(38) should be(3)
      Day3.ringOf(39) should be(3)
      Day3.ringOf(40) should be(3)
      Day3.ringOf(41) should be(3)
      Day3.ringOf(42) should be(3)
      Day3.ringOf(43) should be(3)
      Day3.ringOf(44) should be(3)
      Day3.ringOf(45) should be(3)
      Day3.ringOf(46) should be(3)
      Day3.ringOf(47) should be(3)
      Day3.ringOf(48) should be(3)
      Day3.ringOf(49) should be(3)
    }

    "calculate coordinate" in {
      Day3.coordinateOf(1) should be( (0, 0) )

      Day3.coordinateOf(2) should be( (1, 0) )
      Day3.coordinateOf(3) should be( (1, 1) )
      Day3.coordinateOf(4) should be( (1, 0) )
      Day3.coordinateOf(5) should be( (1, 1) )
      Day3.coordinateOf(6) should be( (1, 0) )
      Day3.coordinateOf(7) should be( (1, 1) )
      Day3.coordinateOf(8) should be( (1, 0) )
      Day3.coordinateOf(9) should be( (1, 1) )

      Day3.coordinateOf(10) should be( (2, 1) )
      Day3.coordinateOf(11) should be( (2, 0) )
      Day3.coordinateOf(12) should be( (2, 1) )
      Day3.coordinateOf(13) should be( (2, 2) )
      Day3.coordinateOf(14) should be( (2, 1) )
      Day3.coordinateOf(15) should be( (2, 0) )
      Day3.coordinateOf(16) should be( (2, 1) )
      Day3.coordinateOf(17) should be( (2, 2) )
      Day3.coordinateOf(18) should be( (2, 1) )
      Day3.coordinateOf(19) should be( (2, 0) )
      Day3.coordinateOf(20) should be( (2, 1) )
      Day3.coordinateOf(21) should be( (2, 2) )
      Day3.coordinateOf(22) should be( (2, 1) )
      Day3.coordinateOf(23) should be( (2, 0) )
      Day3.coordinateOf(24) should be( (2, 1) )
      Day3.coordinateOf(25) should be( (2, 2) )

      Day3.coordinateOf(26) should be( (3, 2) )
      Day3.coordinateOf(27) should be( (3, 1) )
      Day3.coordinateOf(28) should be( (3, 0) )
      Day3.coordinateOf(29) should be( (3, 1) )
      Day3.coordinateOf(30) should be( (3, 2) )
      Day3.coordinateOf(31) should be( (3, 3) )
      Day3.coordinateOf(32) should be( (3, 2) )
      Day3.coordinateOf(33) should be( (3, 1) )
      Day3.coordinateOf(34) should be( (3, 0) )
      Day3.coordinateOf(35) should be( (3, 1) )
      Day3.coordinateOf(36) should be( (3, 2) )
      Day3.coordinateOf(37) should be( (3, 3) )
      Day3.coordinateOf(38) should be( (3, 2) )
      Day3.coordinateOf(39) should be( (3, 1) )
      Day3.coordinateOf(40) should be( (3, 0) )
      Day3.coordinateOf(41) should be( (3, 1) )
      Day3.coordinateOf(42) should be( (3, 2) )
      Day3.coordinateOf(43) should be( (3, 3) )
      Day3.coordinateOf(44) should be( (3, 2) )
      Day3.coordinateOf(45) should be( (3, 1) )
      Day3.coordinateOf(46) should be( (3, 0) )
      Day3.coordinateOf(47) should be( (3, 1) )
      Day3.coordinateOf(48) should be( (3, 2) )
      Day3.coordinateOf(49) should be( (3, 3) )
    }

    "process examples correctly" in {
      Day3.part1(1) should be (0)
      Day3.part1(12) should be (3)
      Day3.part1(23) should be (2)
      Day3.part1(1024) should be(31)
    }
  }

}
