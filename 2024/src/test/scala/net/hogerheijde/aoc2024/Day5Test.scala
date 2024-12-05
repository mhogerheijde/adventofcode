package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day5.Edit
import net.hogerheijde.aoc2024.Day5.Edits
import net.hogerheijde.aoc2024.Day5.PrinterInstructions
import net.hogerheijde.aoc2024.Day5.Rule
import net.hogerheijde.aoc2024.Day5.RuleSet
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47
      |""".stripMargin


  val examplePrinterInstructions: PrinterInstructions = PrinterInstructions(
    RuleSet(
      Seq(
        Rule(47, 53),
        Rule(97, 13),
        Rule(97, 61),
        Rule(97, 47),
        Rule(75, 29),
        Rule(61, 13),
        Rule(75, 53),
        Rule(29, 13),
        Rule(97, 29),
        Rule(53, 29),
        Rule(61, 53),
        Rule(97, 53),
        Rule(61, 29),
        Rule(47, 13),
        Rule(75, 47),
        Rule(97, 75),
        Rule(47, 61),
        Rule(75, 61),
        Rule(47, 29),
        Rule(75, 13),
        Rule(53, 13),
      )
    ),
    Seq(
      Edits(IndexedSeq(75, 47, 61, 53, 29)),
      Edits(IndexedSeq(97, 61, 53, 29, 13)),
      Edits(IndexedSeq(75, 29, 13)),
      Edits(IndexedSeq(75, 97, 47, 61, 53)),
      Edits(IndexedSeq(61, 13, 29)),
      Edits(IndexedSeq(97, 13, 75, 29, 47)),
    ),
  )

  "Day 5 parser" should {
    "parse" in {
      Parser.parse(Day5.rule(_))("47|53\n").get should be(Rule(47, 53))
      Parser.parse(Day5.edits(_))("61,13,29\n97,13,75,29,47\n").get should be(
        Seq(
          Edits(IndexedSeq(61, 13, 29)),
          Edits(IndexedSeq(97, 13, 75, 29, 47)),
        )
      )
    }
  }

  "Edit" should {
    "determine validity" in {
      Edit(75, Set(), Set(47, 61, 53, 29)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(47, Set(75), Set(61, 53, 29)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(61, Set(75, 47), Set(53, 29)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(53, Set(75, 47, 61), Set(29)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(29, Set(75, 47, 61, 53), Set()).isValid(examplePrinterInstructions.rules) should be(true)
    }

    "determine invalidity" in {
      Edit(75, Set(), Set(97, 47, 61, 53)).isValid(examplePrinterInstructions.rules) should be(false)
      Edit(97, Set(75), Set(47, 61, 53)).isValid(examplePrinterInstructions.rules) should be(false)
      Edit(47, Set(97, 75), Set(61, 53)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(61, Set(47, 97, 75), Set(53)).isValid(examplePrinterInstructions.rules) should be(true)
      Edit(53, Set(61, 47, 97, 75), Set()).isValid(examplePrinterInstructions.rules) should be(true)
    }
  }
  
  "Edits" should {
    "break up into seq of Edit" in {
      Edits(IndexedSeq(75, 47, 61, 53, 29))._edits should be (Seq(
        Edit(75, Set(), Set(47, 61, 53, 29)),
        Edit (47, Set(75), Set(61, 53, 29)),
        Edit(61, Set(75, 47), Set(53, 29)),
        Edit(53, Set(75, 47, 61), Set(29)),
        Edit(29, Set(75, 47, 61, 53), Set()),
      )
      )
    }
  }

  "Day 5" should {

    "parse input" in {
      Day5.parse(exampleInput) should be(examplePrinterInstructions)
    }

    "Part1: example answer" in {
      Day5.part1(Day5.parse(exampleInput)) should be(143)
    }

    "Part2: example answer" in {
      Day5.part2(Day5.parse(exampleInput)) should be(0)
    }
  }
}