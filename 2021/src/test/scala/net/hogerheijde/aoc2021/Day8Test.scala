
package net.hogerheijde.aoc2021

import net.hogerheijde.aoc2021.Day8.Display
import net.hogerheijde.aoc2021.Day8.Monitor
import net.hogerheijde.aoc2021.Day8.Note
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day8Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
        |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
        |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
        |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
        |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
        |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
        |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
        |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
        |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
        |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
        |""".stripMargin
    Day8.parse(input)
  }

  "Parser" should {
    "parse single line" in {
      Day8.parse(
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      ) should be (
        Seq(
          Note(
            Seq(
              Display("be"),
              Display("cfbegad"),
              Display("cbdgef"),
              Display("fgaecd"),
              Display("cgeb"),
              Display("fdcge"),
              Display("agebfd"),
              Display("fecdb"),
              Display("fabcd"),
              Display("edb"),
            ),
            Monitor(
              Display("fdgacbe"),
              Display("cefdb"),
              Display("cefbgd"),
              Display("gcbe"),
            )
          )
        )
      )
    }
    "parse example" in {
      exampleInput should have length(10)
    }
  }

  "Model" should {
    "resolve digits" in {
      val example = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
      val note = Day8.parse(example).head
      note.resolveDigits should be (
        Map(
          Display("cagedb") -> 0,
          Display("ab") -> 1,
          Display("gcdfa") -> 2,
          Display("fbcad") -> 3,
          Display("eafb") -> 4,
          Display("cdfbe") -> 5,
          Display("cdfgeb") -> 6,
          Display("dab") -> 7,
          Display("acedgfb") -> 8,
          Display("cefabd") -> 9,
        )
      )
    }
    "resolve value" in {
      val example = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
      val note = Day8.parse(example).head
      note.resolveValue should be (5353)
    }
  }

  "Day 8" should {
    "part 1" in { Day8.part1(exampleInput) should be (26) }
    "part 2" in { Day8.part2(exampleInput) should be (61229) }
  }
}
