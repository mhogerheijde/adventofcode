package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day3.Coordinate
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
      Day3.coordinateOf(1) should be( Coordinate(0, 0) )

      Day3.coordinateOf(2) should be( Coordinate(1, 0) )
      Day3.coordinateOf(3) should be( Coordinate(1, 1) )
      Day3.coordinateOf(4) should be( Coordinate(1, 0) )
      Day3.coordinateOf(5) should be( Coordinate(1, 1) )
      Day3.coordinateOf(6) should be( Coordinate(1, 0) )
      Day3.coordinateOf(7) should be( Coordinate(1, 1) )
      Day3.coordinateOf(8) should be( Coordinate(1, 0) )
      Day3.coordinateOf(9) should be( Coordinate(1, 1) )

      Day3.coordinateOf(10) should be( Coordinate(2, 1) )
      Day3.coordinateOf(11) should be( Coordinate(2, 0) )
      Day3.coordinateOf(12) should be( Coordinate(2, 1) )
      Day3.coordinateOf(13) should be( Coordinate(2, 2) )
      Day3.coordinateOf(14) should be( Coordinate(2, 1) )
      Day3.coordinateOf(15) should be( Coordinate(2, 0) )
      Day3.coordinateOf(16) should be( Coordinate(2, 1) )
      Day3.coordinateOf(17) should be( Coordinate(2, 2) )
      Day3.coordinateOf(18) should be( Coordinate(2, 1) )
      Day3.coordinateOf(19) should be( Coordinate(2, 0) )
      Day3.coordinateOf(20) should be( Coordinate(2, 1) )
      Day3.coordinateOf(21) should be( Coordinate(2, 2) )
      Day3.coordinateOf(22) should be( Coordinate(2, 1) )
      Day3.coordinateOf(23) should be( Coordinate(2, 0) )
      Day3.coordinateOf(24) should be( Coordinate(2, 1) )
      Day3.coordinateOf(25) should be( Coordinate(2, 2) )

      Day3.coordinateOf(26) should be( Coordinate(3, 2) )
      Day3.coordinateOf(27) should be( Coordinate(3, 1) )
      Day3.coordinateOf(28) should be( Coordinate(3, 0) )
      Day3.coordinateOf(29) should be( Coordinate(3, 1) )
      Day3.coordinateOf(30) should be( Coordinate(3, 2) )
      Day3.coordinateOf(31) should be( Coordinate(3, 3) )
      Day3.coordinateOf(32) should be( Coordinate(3, 2) )
      Day3.coordinateOf(33) should be( Coordinate(3, 1) )
      Day3.coordinateOf(34) should be( Coordinate(3, 0) )
      Day3.coordinateOf(35) should be( Coordinate(3, 1) )
      Day3.coordinateOf(36) should be( Coordinate(3, 2) )
      Day3.coordinateOf(37) should be( Coordinate(3, 3) )
      Day3.coordinateOf(38) should be( Coordinate(3, 2) )
      Day3.coordinateOf(39) should be( Coordinate(3, 1) )
      Day3.coordinateOf(40) should be( Coordinate(3, 0) )
      Day3.coordinateOf(41) should be( Coordinate(3, 1) )
      Day3.coordinateOf(42) should be( Coordinate(3, 2) )
      Day3.coordinateOf(43) should be( Coordinate(3, 3) )
      Day3.coordinateOf(44) should be( Coordinate(3, 2) )
      Day3.coordinateOf(45) should be( Coordinate(3, 1) )
      Day3.coordinateOf(46) should be( Coordinate(3, 0) )
      Day3.coordinateOf(47) should be( Coordinate(3, 1) )
      Day3.coordinateOf(48) should be( Coordinate(3, 2) )
      Day3.coordinateOf(49) should be( Coordinate(3, 3) )
    }

    "process examples correctly" in {
      Day3.part1(1) should be (0)
      Day3.part1(12) should be (3)
      Day3.part1(23) should be (2)
      Day3.part1(1024) should be(31)
    }
  }

  "Coordinate" should {
    "know it is in de top right corner" in {
      Range(1, 10).foreach { i=>
        Coordinate(i,   i).isTopRightCorner should be(true)

        Coordinate(i,   i+1).isTopRightCorner should be(false)
        Coordinate(i+1, i).isTopRightCorner should be(false)
        Coordinate(i+1, i-1).isTopRightCorner should be(false)
        Coordinate(i-1, i+1).isTopRightCorner should be(false)
      }
    }

    "know it is in de top left corner" in {
      Range(1, 10).foreach { i=>
        Coordinate(horizontal = -1 * i, vertical = i).isTopLeftCorner should be(true)
        Coordinate(horizontal = -1 * i, vertical = i+1).isTopLeftCorner should be(false)
      }
    }

    "know it is in de bottom left corner" in {
      Range(-1, -10).foreach { i=>
        Coordinate(horizontal = i, vertical = i).isBottomLeftCorner should be(true)
        Coordinate(horizontal = i, vertical = i+1).isBottomLeftCorner should be(false)
      }
    }


    "know it is in de bottom right corner" in {
      Range(1, 10).foreach { i=>
        Coordinate(horizontal = i, vertical = -1 * i).isBottomRightCorner should be(true)
        Coordinate(horizontal = i + 1, vertical = -1 * i).isBottomRightCorner should be(false)
      }
    }

    "know it is left of top right corner" in {
      Coordinate(vertical = 2, horizontal = -3).isLeftOfTopRightCorner should be (false)
      Coordinate(vertical = 2, horizontal = -2).isLeftOfTopRightCorner should be (false)
      Coordinate(vertical = 2, horizontal = -1).isLeftOfTopRightCorner should be (true)
      Coordinate(vertical = 2, horizontal = 0).isLeftOfTopRightCorner should be (true)
      Coordinate(vertical = 2, horizontal = 1).isLeftOfTopRightCorner should be (true)
      Coordinate(vertical = 2, horizontal = 2).isLeftOfTopRightCorner should be (false)
      Coordinate(vertical = 2, horizontal = 3).isLeftOfTopRightCorner should be (false)
    }

    "know it is below of top left corner" in {
      Coordinate(vertical = 3, horizontal = -2).isBelowTopLeftCorner should be (false)
      Coordinate(vertical = 2, horizontal = -2).isBelowTopLeftCorner should be (false)
      Coordinate(vertical = 1, horizontal = -2).isBelowTopLeftCorner should be (true)
      Coordinate(vertical = 0, horizontal = -2).isBelowTopLeftCorner should be (true)
      Coordinate(vertical = -1, horizontal = -2).isBelowTopLeftCorner should be (true)
      Coordinate(vertical = -2, horizontal = -2).isBelowTopLeftCorner should be (false)
      Coordinate(vertical = -3, horizontal = -2).isBelowTopLeftCorner should be (false)
    }

    "know it is right of bottom left corner" in {
      Coordinate(vertical = -2, horizontal = -3).isRightOfBottomLeftCorner should be (false)
      Coordinate(vertical = -2, horizontal = -2).isRightOfBottomLeftCorner should be (false)
      Coordinate(vertical = -2, horizontal = -1).isRightOfBottomLeftCorner should be (true)
      Coordinate(vertical = -2, horizontal = 0).isRightOfBottomLeftCorner should be (true)
      Coordinate(vertical = -2, horizontal = 1).isRightOfBottomLeftCorner should be (true)
      Coordinate(vertical = -2, horizontal = 2).isRightOfBottomLeftCorner should be (false)
      Coordinate(vertical = -2, horizontal = 3).isRightOfBottomLeftCorner should be (false)
    }

    "know it is above bottom right corner" in {
      Coordinate(vertical = -3, horizontal = 2).isAboveBottomRightCorner should be (false)
      Coordinate(vertical = 2, horizontal = 2).isAboveBottomRightCorner should be (false)
      Coordinate(vertical = 1, horizontal = 2).isAboveBottomRightCorner should be (true)
      Coordinate(vertical = 0, horizontal = 2).isAboveBottomRightCorner should be (true)
      Coordinate(vertical = -1, horizontal = 2).isAboveBottomRightCorner should be (true)
      Coordinate(vertical = -2, horizontal = 2).isAboveBottomRightCorner should be (false)
      Coordinate(vertical = -3, horizontal = 2).isAboveBottomRightCorner should be (false)
    }

    "know its next coordinate" in {

      val start = Coordinate.Center
      val pos1  = Coordinate(horizontal =  1, vertical =  0)
      val pos2  = Coordinate(horizontal =  1, vertical =  1)
      val pos3  = Coordinate(horizontal =  0, vertical =  1)
      val pos4  = Coordinate(horizontal = -1, vertical =  1)
      val pos5  = Coordinate(horizontal = -1, vertical =  0)
      val pos6  = Coordinate(horizontal = -1, vertical = -1)
      val pos7  = Coordinate(horizontal =  0, vertical = -1)
      val pos8  = Coordinate(horizontal =  1, vertical = -1)
      val pos9  = Coordinate(horizontal =  2, vertical = -1)
      val pos10 = Coordinate(horizontal =  2, vertical =  0)
      val pos11 = Coordinate(horizontal =  2, vertical =  1)
      val pos12 = Coordinate(horizontal =  2, vertical =  2)
      val pos13 = Coordinate(horizontal =  1, vertical =  2)

      start.next should be (pos1)
      pos1.next should be (pos2)
      pos2.next should be (pos3)
      pos3.next should be (pos4)
      pos4.next should be (pos5)
      pos5.next should be (pos6)
      pos6.next should be (pos7)
      pos7.next should be (pos8)
      pos8.next should be (pos9)
      pos9.next should be (pos10)
      pos10.next should be (pos11)
      pos11.next should be (pos12)
      pos12.next should be (pos13)



    }
    "increases the grid" in {

      val result = Day3.increase(Map(Coordinate.Center -> 1), Coordinate.Center)
      result._1 should be (
        Map(
          Coordinate.Center -> 1,
          Coordinate(vertical = 0, horizontal = 1) -> 1
        )
      )

      result._2 should be (Coordinate(vertical = 0, horizontal = 1))
    }


    "increases the grid 2" in {

      val result = Day3.increase(Map(
        Coordinate.Center -> 1,
        Coordinate(vertical = 0, horizontal = 1) -> 1
      ), Coordinate.Center.next)


      result._1 should be (
        Map(
          Coordinate.Center -> 1,
          Coordinate(vertical = 0, horizontal = 1) -> 1,
          Coordinate(vertical = 1, horizontal = 1) -> 2
        )
      )

      result._2 should be (Coordinate(vertical = 1, horizontal = 1))
    }

    "increases the grid 3" in {

      val result = Day3.increase(Map(
        Coordinate.Center -> 1,
        Coordinate(vertical = 0, horizontal = 1) -> 1,
        Coordinate(vertical = 1, horizontal = 1) -> 2
      ), Coordinate.Center.next.next)


      result._1 should be (
        Map(
          Coordinate.Center -> 1,
          Coordinate(vertical = 0, horizontal = 1) -> 1,
          Coordinate(vertical = 1, horizontal = 1) -> 2,
          Coordinate(vertical = 1, horizontal = 0) -> 4
        )
      )

      result._2 should be (Coordinate(vertical = 1, horizontal = 0))
    }

  }

}
