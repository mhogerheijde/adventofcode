package net.hogerheijde.aoc2018.day16

import net.hogerheijde.aoc.util.Implicits.GroupByHelper
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day16.CPU._
import net.hogerheijde.aoc2018.day16.Program.Input

object Day16 extends Day2018[Input, Int, Int] {
  override def parse(input: String): Input = {
    Parser.parse(Parsers.cpu(_))(input).get
  }

  override def part1(input: Input): Int = {
    val exampleCandidates = input.examples.map { e =>
      val allInstructions = CPU.Instruction.possibleInstructions(e.instruction)
      (e.instruction.opcode, allInstructions.filter(i => i.operate(e.before) == e.after))
    }
    exampleCandidates.count(_._2.size >= 3)
  }

  override def part2(input: Input): Int = {

    val mapping = Map(
      0  -> Eqri,
      1  -> MulR,
      2  -> Gtri,
      3  -> Gtrr,
      4  -> BanR,
      5  -> AddI,
      6  -> SetI,
      7  -> Gtir,
      8  -> MulI,
      9  -> BorI,
      10 -> SetR,
      11 -> AddR,
      12 -> BanI,
      13 -> BorR,
      14 -> Eqir,
      15 -> Eqrr,
    )

    val endState = input.testcases
      .map(tc => mapping(tc._0)(tc._1, tc._2, tc._3))
      .foldLeft(Program.State(0,0,0,0)) { case (currentState, nextInstruction) =>
          nextInstruction.operate(currentState)
    }

    endState._0
  }
}
