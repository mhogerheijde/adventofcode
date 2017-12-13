package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day12.Day12
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day12Test extends WordSpec with Matchers {

  val input = """0 <-> 2
                |1 <-> 1
                |2 <-> 0, 3, 4
                |3 <-> 2, 4
                |4 <-> 2, 3, 6
                |5 <-> 6
                |6 <-> 4, 5""".stripMargin

  val mapping = Day12.parse(input)

  "Day 12" should {
    "parse" in {
      Day12.parse(input) should be (
        Map(
          0 -> Set(2),
          1 -> Set(1),
          2 -> Set(0, 3, 4),
          3 -> Set(2, 4),
          4 -> Set(2, 3, 6),
          5 -> Set(6),
          6 -> Set(4, 5)
        )
      )
    }

    "resolve part 1" in {
      Day12.resolve1(mapping, Set(), 0) should be (Set(0, 2, 3, 4, 5, 6))
    }

    "calculate leftovers" in {
      Day12.leftOver(mapping, 0) should be (Map(1 -> Set(1)))
      Day12.leftOver(mapping, 1) should be (Map(
        0 -> Set(2),
        2 -> Set(0, 3, 4),
        3 -> Set(2, 4),
        4 -> Set(2, 3, 6),
        5 -> Set(6),
        6 -> Set(4, 5)
      ))
    }

    "calculate amount" in {
      Day12.resolve2(mapping, 0, 0) should be(2)

      val mapping2 =  Map(
        0 -> Set(2),
        1 -> Set(1),
        2 -> Set(0, 3, 4),
        3 -> Set(2, 4),
        4 -> Set(2, 3, 6),
        5 -> Set(6),
        6 -> Set(4, 5),
        7 -> Set(8, 9),
        8 -> Set(7),
        9 -> Set(7)
      )

      Day12.resolve2(mapping2, 0, 0) should be(3)
    }
  }

}
