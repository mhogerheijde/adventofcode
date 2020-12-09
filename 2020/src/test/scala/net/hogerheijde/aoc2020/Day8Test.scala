package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2020.ExitCondition.Halted
import net.hogerheijde.aoc2020.ExitCondition.Looped
import net.hogerheijde.aoc2020.Instruction.Acc
import net.hogerheijde.aoc2020.Instruction.Jump
import net.hogerheijde.aoc2020.Instruction.NoOperation
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day8Test extends AnyWordSpec with Matchers {

  val example =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin


  "Day 8" should {
    "parse example" in {
      Day8.parse(example) should be (Seq(
        NoOperation(0),
        Acc(1),
        Jump(4),
        Acc(3),
        Jump(-3),
        Acc(-99),
        Acc(+1),
        Jump(-4),
        Acc(+6),
      ))
    }

    "run example" in {
      Day8.run(Day8.parse(example)) should be (Looped(5))
    }

    "mutator should mutate" in {
      val mutator = new Day8.Mutator(Day8.parse(example))
      mutator.next() should be (Seq(
        Jump(0),
        Acc(1),
        Jump(4),
        Acc(3),
        Jump(-3),
        Acc(-99),
        Acc(+1),
        Jump(-4),
        Acc(+6),
      ))

      mutator.next() should be (Seq(
        NoOperation(0),
        Acc(1),
        NoOperation(4),
        Acc(3),
        Jump(-3),
        Acc(-99),
        Acc(+1),
        Jump(-4),
        Acc(+6),
      ))

      mutator.next() should be (Seq(
        NoOperation(0),
        Acc(1),
        Jump(4),
        Acc(3),
        NoOperation(-3),
        Acc(-99),
        Acc(+1),
        Jump(-4),
        Acc(+6),
      ))

      mutator.next() should be (Seq(
        NoOperation(0),
        Acc(1),
        Jump(4),
        Acc(3),
        Jump(-3),
        Acc(-99),
        Acc(+1),
        NoOperation(-4),
        Acc(+6),
      ))

      mutator.hasNext should be (false)

    }


    "run mutated example" in {
      val mutated =
        """nop +0
          |acc +1
          |jmp +4
          |acc +3
          |jmp -3
          |acc -99
          |acc +1
          |nop -4
          |acc +6""".stripMargin
      Day8.run(Day8.parse(mutated)) should be (Halted(8))
    }
    "solve part 2" in {
      Day8.part2(Day8.parse(example)) should be (8)
    }

  }

}
