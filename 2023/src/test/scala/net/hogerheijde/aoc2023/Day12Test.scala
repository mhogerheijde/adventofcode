package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day12.Group.Broken
import net.hogerheijde.aoc2023.Day12.Group.Unknown
import net.hogerheijde.aoc2023.Day12.Group.Working
import net.hogerheijde.aoc2023.Day12.Pattern
import net.hogerheijde.aoc2023.Day12.u
import net.hogerheijde.aoc2023.Day12.b
import net.hogerheijde.aoc2023.Day12.toPattern
import net.hogerheijde.aoc2023.Day12.w
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day12Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1
      |""".stripMargin


  "Day 12 parser" should {
    "parse pattern" in {
       Parser.parse(Day12.pattern)("##??..").get should be (
         Pattern(Seq(Broken(2), Unknown(2), Working(2)))
       )
    }
  }


  "Pattern" should {

    "canonicalise" in {
      // Empty
      Pattern().canonical should be (Pattern())

      // Single part
      Pattern(Seq(1.u)).canonical should be ("?".toPattern)
      Pattern(Seq(2.u)).canonical should be("??".toPattern)
      Pattern(Seq(3.w)).canonical should be("...".toPattern)

      Pattern(Seq(1.w, 1.u)).canonical should be(".?".toPattern)
      Pattern(Seq(1.u, 1.u, 1.u)).canonical should be ("???".toPattern)
      Pattern(Seq(1.u, 1.w, 1.u, 1.u)).canonical should be ("?.??".toPattern)
      Pattern(Seq(1.u, 1.u, 1.u, 1.w, 1.b, 1.b)).canonical should be ("???.##".toPattern)
    }

    "expand for simple cases" in {
      Day12.optionsFor("....".toPattern, Seq()) should be (Seq("....".toPattern))
      Day12.optionsFor(".?..".toPattern, Seq()) should be (Seq("....".toPattern))

      Day12.optionsFor(".#..".toPattern, Seq()) should be (Seq())
      Day12.optionsFor(".#.?.".toPattern, Seq()) should be (Seq())

      Day12.optionsFor(".#..".toPattern, Seq(1)) should be (Seq(".#..".toPattern))
      Day12.optionsFor("###".toPattern, Seq(3)) should be (Seq("###".toPattern))
      Day12.optionsFor("..###".toPattern, Seq(3)) should be (Seq("..###".toPattern))
      Day12.optionsFor("###..".toPattern, Seq(3)) should be (Seq("###..".toPattern))
      Day12.optionsFor("..###..".toPattern, Seq(3)) should be (Seq("..###..".toPattern))

      Day12.optionsFor(".?..".toPattern, Seq(1)) should be(Seq(".#..".toPattern))
//      Day12.optionsFor(".#.?.".toPattern, Seq(1, 1)) should be(Seq(".#.?.".toPattern))

    }

  }

  "Day 12" should {


    "parse input" in {
      Day12.parse(exampleInput) should be(
        Seq(
          (Pattern(Seq(3.u, 1.w, 3.b)), Seq(1, 1, 3)),
          (Pattern(Seq(1.w, 2.u, 2.w, 2.u, 3.w, 1.u, 2.b, 1.w)), Seq(1, 1, 3)),
          (Pattern(Seq(1.u, 1.b, 1.u, 1.b, 1.u, 1.b, 1.u, 1.b, 1.u, 1.b, 1.u, 1.b, 1.u, 1.b, 1.u)), Seq(1, 3, 1, 6)),
          (Pattern(Seq(4.u, 1.w, 1.b, 3.w, 1.b, 3.w)), Seq(4, 1, 1)),
          (Pattern(Seq(4.u, 1.w, 6.b, 2.w, 5.b, 1.w)), Seq(1, 6, 5)),
          (Pattern(Seq(1.u, 3.b, 8.u)), Seq(3, 2, 1)),
        )
      )
    }

    "Part1: example answer" in {
      Day12.part1(Day12.parse(exampleInput)) should be(0)
    }

    "Part2: example answer" in {
      Day12.part2(Day12.parse(exampleInput)) should be(0)
    }
  }
}