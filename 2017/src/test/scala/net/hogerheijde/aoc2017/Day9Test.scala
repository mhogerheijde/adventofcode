package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day9.Day9
import net.hogerheijde.aoc2017.day9.Garbage
import net.hogerheijde.aoc2017.day9.Group
import net.hogerheijde.aoc2017.day9.Tokens
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq


class Day9Test extends WordSpec with Matchers {

  val input1 = "{}"
  val input2 = "{{{}}}"
  val input3 = "{{},{}}"
  val input4 = "{{{},{},{{}}}}"
  val input5 = "{<{},{},{{}}>}"
  val input6 = "{<a>,<a>,<a>,<a>}"
  val input7 = "{{<a>},{<a>},{<a>},{<a>}}"
  val input8 = "{{<!>},{<!>},{<!>},{<a>}}"

  val input9 = "{{<a}b>},{<ab>},{<ab>},{<ab>}}"
  val input10 = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  val input11 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

  val parse1 = Day9.parse(input1)

  "Day 9" should {
    "parse input" in {
      Day9.parse(input1) should be (Group(1))
      Day9.parse(input2) should be (Group(1, Group(2, Group(3))))
      Day9.parse(input3) should be (Group(1, Group(2), Group(2)))
      Day9.parse(input4) should be (Group(1, Group(2, Group(3), Group(3), Group(3, Group(4)))))
      Day9.parse(input5) should be (Group(1, Garbage(2, 10)))
      Day9.parse(input6) should be (Group(1, Garbage(2, 1), Garbage(2, 1), Garbage(2, 1), Garbage(2, 1)))
      Day9.parse(input7) should be (Group(1, Group(2, Garbage(3, 1)), Group(2, Garbage(3, 1)), Group(2, Garbage(3, 1)), Group(2, Garbage(3, 1))))
      Day9.parse(input8) should be (Group(1, Group(2, Garbage(3, 13))))
      Day9.parse(input9) should be (Group(1, Group(2, Garbage(3, 3)), Group(2, Garbage(3, 2)), Group(2, Garbage(3, 2)), Group(2, Garbage(3, 2))))
      Day9.parse(input10) should be (Group(1, Group(2, Garbage(3, 0)), Group(2, Garbage(3, 0)), Group(2, Garbage(3, 0)), Group(2, Garbage(3, 0))))
      Day9.parse(input11) should be (Group(1, Group(2, Garbage(3, 17))))


//      val list = "{{<!><>}{}}".split("").map(_.toCharArray.head).toList
//      Day9.processGroup_(list) should be (IndexedSeq(Group(0, Group(1, Garbage(2), Garbage(2)), Group(1))))


    }


    "solve part 1" in {
      Day9.part1(Day9.parse("{}")) should be (1)
      Day9.part1(Day9.parse("{{{}}}")) should be (6)
      Day9.part1(Day9.parse("{{},{}}")) should be (5)
      Day9.part1(Day9.parse("{{{},{},{{}}}}")) should be (16)
      Day9.part1(Day9.parse("{<a>,<a>,<a>,<a>}")) should be (1)
      Day9.part1(Day9.parse("{{<ab>},{<ab>},{<ab>},{<ab>}}")) should be (9)
      Day9.part1(Day9.parse("{{<!!>},{<!!>},{<!!>},{<!!>}}")) should be (9)
      Day9.part1(Day9.parse("{{<a!>},{<a!>},{<a!>},{<ab>}}")) should be (3)
    }

    "solve part 2" in {
      Day9.part2(parse1) should be ( () )
    }

  }

}
