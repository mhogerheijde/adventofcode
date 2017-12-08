package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day8.Cpu
import net.hogerheijde.aoc2017.day8.Cpu.Bigger
import net.hogerheijde.aoc2017.day8.Cpu.BiggerOrEqual
import net.hogerheijde.aoc2017.day8.Cpu.Condition
import net.hogerheijde.aoc2017.day8.Cpu.Decrement
import net.hogerheijde.aoc2017.day8.Cpu.Equal
import net.hogerheijde.aoc2017.day8.Cpu.Increment
import net.hogerheijde.aoc2017.day8.Cpu.Instruction
import net.hogerheijde.aoc2017.day8.Cpu.Smaller
import net.hogerheijde.aoc2017.day8.Day8
import net.hogerheijde.aoc2017.day8.Running
import net.hogerheijde.aoc2017.day8.State
import org.scalatest.Matchers
import org.scalatest.WordSpec


class Day8Test extends WordSpec with Matchers {

  val input =
    """b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10
    """.stripMargin.trim

  val inputState = Day8.parse(input)


  "Day8" should {
    "parse input" in {

      val expect = State(List(
        Instruction(Increment("b", 5), Condition("a", Bigger(1))),
        Instruction(Increment("a", 1), Condition("b", Smaller(5))),
        Instruction(Decrement("c", -10), Condition("a", BiggerOrEqual(1))),
        Instruction(Increment("c", -20), Condition("c", Equal(10)))
      ))

      Day8.parse(input) should be(expect)
    }

    "calculate next state" in {

      val expect1 = Running(Cpu(), List(
        Instruction(Increment("a", 1), Condition("b", Smaller(5))),
        Instruction(Decrement("c", -10), Condition("a", BiggerOrEqual(1))),
        Instruction(Increment("c", -20), Condition("c", Equal(10)))
      ), 0)


      val expect2 = Running(Cpu(Map("a" -> 1)), List(
        Instruction(Decrement("c", -10), Condition("a", BiggerOrEqual(1))),
        Instruction(Increment("c", -20), Condition("c", Equal(10)))
      ), 1)

      val next1 = inputState.next
      val next2 = next1.next

      next1 should be (expect1)
      next2 should be (expect2)

    }

    "solve part 1" in {
      Day8.part1(inputState) should be (1)
    }

    "solve part 2" in {
      Day8.part2(inputState) should be (10)
    }

  }

}
