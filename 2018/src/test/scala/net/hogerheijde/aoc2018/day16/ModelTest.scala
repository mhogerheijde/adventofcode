package net.hogerheijde.aoc2018.day16

import net.hogerheijde.aoc2018.day16.CPU.AddI
import net.hogerheijde.aoc2018.day16.CPU.AddR
import net.hogerheijde.aoc2018.day16.CPU.BanI
import net.hogerheijde.aoc2018.day16.CPU.BanR
import net.hogerheijde.aoc2018.day16.CPU.BorI
import net.hogerheijde.aoc2018.day16.CPU.BorR
import net.hogerheijde.aoc2018.day16.CPU.Eqir
import net.hogerheijde.aoc2018.day16.CPU.Eqri
import net.hogerheijde.aoc2018.day16.CPU.Eqrr
import net.hogerheijde.aoc2018.day16.CPU.Gtir
import net.hogerheijde.aoc2018.day16.CPU.Gtri
import net.hogerheijde.aoc2018.day16.CPU.Gtrr
import net.hogerheijde.aoc2018.day16.CPU.MulI
import net.hogerheijde.aoc2018.day16.CPU.MulR
import net.hogerheijde.aoc2018.day16.CPU.SetI
import net.hogerheijde.aoc2018.day16.CPU.SetR
import net.hogerheijde.aoc2018.day16.Program.State
import org.scalatest.Matchers
import org.scalatest.WordSpec

class ModelTest extends WordSpec with Matchers {

  "CPU" should {

    "have working AddR opcode" in {
      AddR(0, 1, 0).operate(State(1, 2, 3, 4)) should be(State(3, 2, 3, 4))
      AddR(1, 1, 1).operate(State(1, 2, 3, 4)) should be(State(1, 4, 3, 4))
      AddR(2, 1, 2).operate(State(1, 2, 3, 4)) should be(State(1, 2, 5, 4))
      AddR(3, 1, 3).operate(State(1, 2, 3, 4)) should be(State(1, 2, 3, 6))
    }


    "have working AddI opcode" in {
      AddI(0, 10, 0).operate(State(1, 2, 3, 4)) should be(State(11, 2, 3, 4))
      AddI(1, 10, 1).operate(State(1, 2, 3, 4)) should be(State(1, 12, 3, 4))
      AddI(2, 10, 2).operate(State(1, 2, 3, 4)) should be(State(1, 2, 13, 4))
      AddI(3, 10, 3).operate(State(1, 2, 3, 4)) should be(State(1, 2, 3, 14))
    }

    "have working MulR opcode" in {
      // 170 dec = 1010 1010 bin
      // 85 dec  = 0101 0101 bin
      // 255 dec = 1111 1111 bin
      // 0 dec   = 0000 0000 bin
      // 240 dec = 1111 0000 bin
      // 15 dec  = 0000 1111 bin
      MulR(0, 1, 0).operate(State(1, 2, 3, 4)) should be(State(2, 2, 3, 4))
      MulR(1, 1, 1).operate(State(1, 2, 3, 4)) should be(State(1, 4, 3, 4))
      MulR(2, 1, 2).operate(State(1, 2, 3, 4)) should be(State(1, 2, 6, 4))
      MulR(3, 1, 3).operate(State(1, 2, 3, 4)) should be(State(1, 2, 3, 8))
    }

    // 170 dec = 1010 1010 bin
    // 85 dec  = 0101 0101 bin
    // 255 dec = 1111 1111 bin
    // 0 dec   = 0000 0000 bin
    // 240 dec = 1111 0000 bin
    // 15 dec  = 0000 1111 bin


    "have working BanI opcode" in {
      BanI(0, 255, 0).operate(State(170, 2, 3, 4)) should be(State(170, 2, 3, 4))
      BanI(0, 255, 1).operate(State(85, 2, 3, 4)) should be(State(85, 85, 3, 4))
      BanI(0, 255, 2).operate(State(240, 2, 3, 4)) should be(State(240, 2, 240, 4))
      BanI(0, 255, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 15))

      BanI(0, 0, 0).operate(State(170, 2, 3, 4)) should be(State(0, 2, 3, 4))
      BanI(0, 0, 1).operate(State(85, 2, 3, 4)) should be(State(85, 0, 3, 4))
      BanI(0, 0, 2).operate(State(240, 2, 3, 4)) should be(State(240, 2, 0, 4))
      BanI(0, 0, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 0))
    }

    "have working BanR opcode" in {
      BanR(0, 1, 0).operate(State(170, 255, 3, 4)) should be(State(170, 255, 3, 4))
      BanR(0, 2, 1).operate(State(85, 2, 255, 4)) should be(State(85, 85, 255, 4))
      BanR(0, 3, 2).operate(State(240, 2, 2, 255)) should be(State(240, 2, 240, 255))
      BanR(0, 0, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 15))
    }

    "have working BorI opcode" in {
      BorI(0, 255, 0).operate(State(170, 2, 3, 4)) should be(State(255, 2, 3, 4))
      BorI(0, 255, 1).operate(State(85, 2, 3, 4)) should be(State(85, 255, 3, 4))
      BorI(0, 255, 2).operate(State(240, 2, 3, 4)) should be(State(240, 2, 255, 4))
      BorI(0, 255, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 255))

      BorI(0, 0, 0).operate(State(170, 2, 3, 4)) should be(State(170, 2, 3, 4))
      BorI(0, 0, 1).operate(State(85, 2, 3, 4)) should be(State(85, 85, 3, 4))
      BorI(0, 0, 2).operate(State(240, 2, 3, 4)) should be(State(240, 2, 240, 4))
      BorI(0, 0, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 15))
    }

    "have working BorR opcode" in {
      BorR(0, 1, 0).operate(State(170, 255, 3, 4)) should be(State(255, 255, 3, 4))
      BorR(0, 2, 1).operate(State(85, 2, 255, 4)) should be(State(85, 255, 255, 4))
      BorR(0, 3, 2).operate(State(240, 2, 2, 255)) should be(State(240, 2, 255, 255))
      BorR(0, 0, 3).operate(State(15, 2, 3, 4)) should be(State(15, 2, 3, 15))
    }

    "have working SetI opcode" in {
      SetI(10, 0, 0).operate(State(0, 0, 0, 0)) should be (State(10, 0, 0, 0))
      SetI(10, 0, 1).operate(State(0, 0, 0, 0)) should be (State(0, 10, 0, 0))
      SetI(10, 0, 2).operate(State(0, 0, 0, 0)) should be (State(0, 0, 10, 0))
      SetI(10, 0, 3).operate(State(0, 0, 0, 0)) should be (State(0, 0, 0, 10))
    }

    "have working SetR opcode" in {
      SetR(0, 0, 1).operate(State(10, 0, 0, 0)) should be (State(10, 10, 0, 0))
      SetR(1, 0, 2).operate(State(0, 10, 0, 0)) should be (State(0, 10, 10, 0))
      SetR(2, 0, 3).operate(State(0, 0, 10, 0)) should be (State(0, 0, 10, 10))
      SetR(3, 0, 0).operate(State(0, 0, 0, 10)) should be (State(10, 0, 0, 10))
    }

    "have working Gtir opcode" in {
      Gtir(1, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Gtir(2, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Gtir(3, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
      Gtir(4, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
    }

    "have working Gtri opcode" in {
      Gtri(1, 0, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
      Gtri(1, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
      Gtri(1, 2, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Gtri(1, 3, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
    }

    "have working Gtrr opcode" in {
      Gtrr(0, 1, 3).operate(State(1, 2, 0, 9)) should be (State(1, 2, 0, 0))
      Gtrr(0, 1, 3).operate(State(2, 2, 0, 9)) should be (State(2, 2, 0, 0))
      Gtrr(0, 1, 3).operate(State(3, 2, 0, 9)) should be (State(3, 2, 0, 1))
      Gtrr(0, 1, 3).operate(State(4, 2, 0, 9)) should be (State(4, 2, 0, 1))
    }

    "have working eqir opcode" in {
      Eqir(1, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Eqir(2, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
      Eqir(3, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Eqir(4, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
    }

    "have working eqri opcode" in {
      Eqri(1, 0, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Eqri(1, 1, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
      Eqri(1, 2, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 1))
      Eqri(1, 3, 3).operate(State(0, 2, 0, 9)) should be (State(0, 2, 0, 0))
    }

    "have working eqrr opcode" in {
      Eqrr(0, 1, 3).operate(State(1, 2, 0, 9)) should be (State(1, 2, 0, 0))
      Eqrr(0, 1, 3).operate(State(2, 2, 0, 9)) should be (State(2, 2, 0, 1))
      Eqrr(0, 1, 3).operate(State(3, 2, 0, 9)) should be (State(3, 2, 0, 0))
      Eqrr(0, 1, 3).operate(State(4, 2, 0, 9)) should be (State(4, 2, 0, 0))
    }



  }

}
