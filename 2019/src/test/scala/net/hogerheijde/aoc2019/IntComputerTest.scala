package net.hogerheijde.aoc2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec




class IntComputerTest extends AnyWordSpec with Matchers {

  "IntComputer" should {
    "run example of part2" in {
      IntComputer(IndexedSeq(1,1,1,4,99,5,6,0,99)).calculate() should be (IndexedSeq(30,1,1,4,2,5,6,0,99))
      IntComputer(IndexedSeq(1,0,0,0,99)).calculate() should be (IndexedSeq(2,0,0,0,99))
      IntComputer(IndexedSeq(2,3,0,3,99)).calculate() should be (IndexedSeq(2,3,0,6,99))
      IntComputer(IndexedSeq(2,4,4,5,99,0)).calculate() should be (IndexedSeq(2,4,4,5,99,9801))
    }
  }
}
