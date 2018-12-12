package net.hogerheijde.aoc2018.day10

import net.hogerheijde.aoc.common.model.Coordinate
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day10Test extends WordSpec with Matchers {

  val exampleInput = Field.parse(
    """position=< 9,  1> velocity=< 0,  2>
      |position=< 7,  0> velocity=<-1,  0>
      |position=< 3, -2> velocity=<-1,  1>
      |position=< 6, 10> velocity=<-2, -1>
      |position=< 2, -4> velocity=< 2,  2>
      |position=<-6, 10> velocity=< 2, -2>
      |position=< 1,  8> velocity=< 1, -1>
      |position=< 1,  7> velocity=< 1,  0>
      |position=<-3, 11> velocity=< 1, -2>
      |position=< 7,  6> velocity=<-1, -1>
      |position=<-2,  3> velocity=< 1,  0>
      |position=<-4,  3> velocity=< 2,  0>
      |position=<10, -3> velocity=<-1,  1>
      |position=< 5, 11> velocity=< 1, -2>
      |position=< 4,  7> velocity=< 0, -1>
      |position=< 8, -2> velocity=< 0,  1>
      |position=<15,  0> velocity=<-2,  0>
      |position=< 1,  6> velocity=< 1,  0>
      |position=< 8,  9> velocity=< 0, -1>
      |position=< 3,  3> velocity=<-1,  1>
      |position=< 0,  5> velocity=< 0, -1>
      |position=<-2,  2> velocity=< 2,  0>
      |position=< 5, -2> velocity=< 1,  2>
      |position=< 1,  4> velocity=< 2,  1>
      |position=<-2,  7> velocity=< 2, -2>
      |position=< 3,  6> velocity=<-1, -1>
      |position=< 5,  0> velocity=< 1,  0>
      |position=<-6,  0> velocity=< 2,  0>
      |position=< 5,  9> velocity=< 1, -2>
      |position=<14,  7> velocity=<-2,  0>
      |position=<-3,  6> velocity=< 2, -1>
    """.stripMargin)


  "Field" should {
    "parse example input" in {
      Field.parse(input =
        """position=< 9,  1> velocity=< 0,  2>
          |position=< 7,  0> velocity=<-1,  0>
          |position=< 3, -2> velocity=<-1,  1>""".stripMargin) should be (
        Field(IndexedSeq(
          Point(Coordinate(9, 1), Velocity(0, 2)),
          Point(Coordinate(7, 0), Velocity(-1, 0)),
          Point(Coordinate(3, -2), Velocity(-1, 1)),
        ))
      )
    }

    "should be pretty" in {
      exampleInput.pretty should be (
        """........#.............
          |................#.....
          |.........#.#..#.......
          |......................
          |#..........#.#.......#
          |...............#......
          |....#.................
          |..#.#....#............
          |.......#..............
          |......#...............
          |...#...#.#...#........
          |....#..#..#.........#.
          |.......#..............
          |...........#..#.......
          |#...........#.........
          |...#.......#..........""".stripMargin)
    }

    "should advance 1 second" in {
      exampleInput.advance.pretty should be (
        """........#....#....
          |......#.....#.....
          |#.........#......#
          |..................
          |....#.............
          |..##.........#....
          |....#.#...........
          |...##.##..#.......
          |......#.#.........
          |......#...#.....#.
          |#...........#.....
          |..#.....#.#.......""".stripMargin)
    }

    "should show HI after 3 seconds" in {
      exampleInput.advance.advance.advance.pretty should be (
        """#...#..###
          |#...#...#.
          |#...#...#.
          |#####...#.
          |#...#...#.
          |#...#...#.
          |#...#...#.
          |#...#..###""".stripMargin)
    }

     "should know it shows a message after 3 seconds" in {
       val second1 = exampleInput.advance
       val second2 = second1.advance
       val second3 = second2.advance
       exampleInput.showsMessage should be(false)
       second1.showsMessage should be(false)
       second2.showsMessage should be(false)
       second3.showsMessage should be(true)
    }

  }

  "Day 10" should {
    "solve example input" in {
      Day10.part1(exampleInput) should be (
        """#...#..###
          |#...#...#.
          |#...#...#.
          |#####...#.
          |#...#...#.
          |#...#...#.
          |#...#...#.
          |#...#..###""".stripMargin
      )
    }
  }

}
