package net.hogerheijde.aoc2025

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2025.Day1.Dial
import net.hogerheijde.aoc2025.Day1.Instruction
import net.hogerheijde.aoc2025.Day1.Instruction.Right
import net.hogerheijde.aoc2025.Day1.Instruction.Left
import net.hogerheijde.aoc2025.Day1.instructions
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day1Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82
      |""".stripMargin

  val exampleModel: Seq[Instruction] =
    Seq(
      Left(68),
      Left(30),
      Right(48),
      Left(5),
      Right(60),
      Left(55),
      Left(1),
      Left(99),
      Right(14),
      Left(82),
    )

  "Day 1 parser" should {
    "parse" in {
      Parser.parse(instructions(_))(exampleInput).get should be (exampleModel)
    }
  }

  "Dial" should {
    "rotate left beyond zero" in {
      Dial(50, 100).rotate(Left(68))._1.position should be (82)
      Dial(0, 100).rotate(Left(1))._1.position should be(99)
    }

    "rotate left" in {
      Dial(82, 100).rotate(Left(30))._1.position should be (52)
    }

    "rotate right beyond 99" in {
      Dial(95, 100).rotate(Right(60))._1.position should be(55)
      Dial(99, 100).rotate(Right(1))._1.position should be(0)
    }

    "rotate right" in {
      Dial(50, 100).rotate(Right(210))._1.position should be(60)
      Dial(50, 100).rotate(Left(210))._1.position should be(40)
    }

    "rotate bigger than size" in {
      Dial(50, 100).rotate(Right(30))._1.position should be(80)
    }

    "hit zero count left" in {
      Dial(50, 100).rotate(Left(68))._2 should be(1)
      Dial(50, 100).rotate(Left(50))._2 should be(1)
      Dial(99, 100).rotate(Left(99))._2 should be(1)
      Dial(0, 100).rotate(Left(5))._2 should be(0)
      Dial(0, 100).rotate(Left(100))._2 should be(1)
    }

    "hit zero count right" in {
      Dial(95, 100).rotate(Right(60))._2 should be(1)
      Dial(14, 100).rotate(Right(85))._2 should be(0)
      Dial(14, 100).rotate(Right(86))._2 should be(1)
      Dial(14, 100).rotate(Right(87))._2 should be(1)
    }

  }

  "Day 1" should {

    "parse input" in {
      Day1.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day1.part1(Day1.parse(exampleInput)) should be(3)
    }

    "Part2: example answer" in {
      Day1.part2(Day1.parse(exampleInput)) should be(6)
    }
  }
}