package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day7.Expression
import net.hogerheijde.aoc2024.Day7.Operator.Concat
import net.hogerheijde.aoc2024.Day7.Operator.Plus
import net.hogerheijde.aoc2024.Day7.Operator.Times
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day7Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20
      |""".stripMargin



  "Day 7 parser" should {
    "parse" in {
      // Parser.parse(...)("...").get should be ("")
    }
  }


  "Expression" should {
    "resolve" in {
      Expression(190, Seq(10, 19)).resolve(Seq(Plus)) should be (29)
      Expression(190, Seq(10, 19)).resolve(Seq(Times)) should be (190)

      Expression(3267, Seq(81, 40, 27)).resolve(Seq(Plus, Plus)) should be (148)
      Expression(3267, Seq(81, 40, 27)).resolve(Seq(Plus, Times)) should be (3267)
      Expression(3267, Seq(81, 40, 27)).resolve(Seq(Times, Plus)) should be (3267)
      Expression(3267, Seq(81, 40, 27)).resolve(Seq(Times, Times)) should be (87480)
    }

    "check satisfaction" in {
      Expression(190, Seq(10, 19)).isSatisfiedBy(Seq(Plus)) should be(false)
      Expression(190, Seq(10, 19)).isSatisfiedBy(Seq(Times)) should be(true)
      Expression(156, Seq(15, 6)).isSatisfiedBy(Seq(Concat)) should be (true)
      Expression(7290, Seq(6, 8, 6, 15)).isSatisfiedBy(Seq(Times, Concat, Times)) should be (true)
      Expression(192, Seq(17, 8, 14)).isSatisfiedBy(Seq(Concat, Plus)) should be (true)
    }
    "check possible satisfaction" in {
      val ops = Seq(Plus, Times)

      Expression(190, Seq(10, 19)).canBeSatisfied(ops) should be (true)
      Expression(3267, Seq(81, 40, 27)).canBeSatisfied(ops) should be (true)
      Expression(83, Seq(17, 5)).canBeSatisfied(ops) should be (false)
      Expression(156, Seq(15, 6)).canBeSatisfied(ops) should be (false)
      Expression(7290, Seq(6, 8, 6, 15)).canBeSatisfied(ops) should be (false)
      Expression(161011, Seq(16, 10, 13)).canBeSatisfied(ops) should be (false)
      Expression(192, Seq(17, 8, 14)).canBeSatisfied(ops) should be (false)
      Expression(21037, Seq(9, 7, 18, 13)).canBeSatisfied(ops) should be (false)
      Expression(292, Seq(11, 6, 16, 20)).canBeSatisfied(ops) should be (true)
    }
  }

  "Day 7" should {

    "generate sequence" in {
      val iterator = Day7.combinationsWithRepeats(2, Seq(Plus, Times))

      iterator.toSeq should be (
        Seq(
          Seq(Plus, Plus),
          Seq(Plus, Times),
          Seq(Times, Plus),
          Seq(Times, Times),
        )
      )
    }

    "parse input" in {
      Day7.parse(exampleInput) should be(
        Seq(
          Expression(190, Seq(10, 19)),
          Expression(3267, Seq(81, 40, 27)),
          Expression(83, Seq(17, 5)),
          Expression(156, Seq(15, 6)),
          Expression(7290, Seq(6, 8, 6, 15)),
          Expression(161011, Seq(16, 10, 13)),
          Expression(192, Seq(17, 8, 14)),
          Expression(21037, Seq(9, 7, 18, 13)),
          Expression(292, Seq(11, 6, 16, 20)),
        )
      )
    }

    "Part1: example answer" in {
      Day7.part1(Day7.parse(exampleInput)) should be(3749)
    }

    "Part2: example answer" in {
      Day7.part2(Day7.parse(exampleInput)) should be(11387)
    }
  }
}