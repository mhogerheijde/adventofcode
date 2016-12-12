package net.hogerheijde.aoc2016.days.day2.model

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class InstructionTest extends FlatSpec with Matchers {

  "Instruction" should "parse L correctly" in {
    Instruction.fromChar('L').get should be(GoLeft)
  }

  it should "parse R correctly" in {
    Instruction.fromChar('R').get should be(GoRight)
  }

  it should "parse U correctly" in {
    Instruction.fromChar('U').get should be(GoUp)
  }

  it should "parse D correctly" in {
    Instruction.fromChar('D').get should be(GoDown)
  }

  it should "parse unknowns into None" in {
    Instruction.fromChar('a') should be(None)
    Instruction.fromChar('F') should be(None)
    Instruction.fromChar('Q') should be(None)
    Instruction.fromChar('z') should be(None)
    Instruction.fromChar('.') should be(None)
    Instruction.fromChar('/') should be(None)
    Instruction.fromChar('\\') should be(None)
    Instruction.fromChar('8') should be(None)
    Instruction.fromChar('3') should be(None)
  }

}
