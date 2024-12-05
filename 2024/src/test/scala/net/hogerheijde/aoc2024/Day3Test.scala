package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day3.multiply
import net.hogerheijde.aoc2024.Day3.garbage
import net.hogerheijde.aoc2024.Day3.program
import net.hogerheijde.aoc2024.Day3.Instruction
import net.hogerheijde.aoc2024.Day3.Instruction.Garbage
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Do
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Dont
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Multiply
import net.hogerheijde.aoc2024.Day3.Model
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  val exampleInputPt2: String =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  val exampleProgram = Seq(
    Garbage("x"),
    Multiply(2, 4),
    Garbage("%&mul[3,7]!@^do_not_"),
    Multiply(5, 5),
    Garbage("+mul(32,64]then("),
    Multiply(11, 8),
    Multiply(8, 5),
    Garbage(")"),
  )

  val exampleProgramPt2 = Seq(
    Garbage("x"),
    Multiply(2, 4),
    Garbage("&mul[3,7]!^"),
    Dont,
    Garbage("_"),
    Multiply(5, 5),
    Garbage("+mul(32,64]("),
    Multiply(11, 8),
    Garbage("un"),
    Do,
    Garbage("?"),
    Multiply(8, 5),
    Garbage(")"),
  )

  "Day 3 parser" should {
    "parse" in {
      Parser.parse(multiply(_))("mul(2,4)%&mul[3,7]").get should be(Multiply(2, 4))
      Parser.parse(garbage(_))("foobarbaz mul(2,4)%&mul[3,7]").get should be(Garbage("foobarbaz "))
      Parser.parse(garbage(_))("xmul(2,4)%&mul[3,7]!@^do_not_mul(").get should be (Garbage("x"))
      Parser.parse(program(_))(exampleInput).get should be (exampleProgram)
      Parser.parse(program(_))(exampleInputPt2).get should be(exampleProgramPt2)

    }
  }

  "Day 3" should {
    "parse input" in {
      Day3.parse(exampleInput) should be(exampleProgram)
    }

    "Part1: example answer" in {
      Day3.part1(Day3.parse(exampleInput)) should be(161)
    }

    "Part2: example answer" in {
      Day3.part2(Day3.parse(exampleInputPt2)) should be(48)
    }
  }
}