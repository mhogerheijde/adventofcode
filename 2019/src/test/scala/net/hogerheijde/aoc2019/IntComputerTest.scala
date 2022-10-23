package net.hogerheijde.aoc2019

import net.hogerheijde.aoc2019.IntComputer.Data
import net.hogerheijde.aoc2019.IntComputer.Memory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec




class IntComputerTest extends AnyWordSpec with Matchers {

  "IntComputer" should {
    "run example of Day 2" in {
      IntComputer(IndexedSeq(1,1,1,4,99,5,6,0,99)).calculate() should be (IndexedSeq(30,1,1,4,2,5,6,0,99))
      IntComputer(IndexedSeq(1,0,0,0,99)).calculate() should be (IndexedSeq(2,0,0,0,99))
      IntComputer(IndexedSeq(2,3,0,3,99)).calculate() should be (IndexedSeq(2,3,0,6,99))
      IntComputer(IndexedSeq(2,4,4,5,99,0)).calculate() should be (IndexedSeq(2,4,4,5,99,9801))
    }
    "run example of Day 5" in {
      IntComputer(IndexedSeq(1002,4,3,4,33)).calculate() should be (IndexedSeq(1002,4,3,4,99))

      val equals8Positional = IntComputer(Memory(3,9,8,9,10,9,4,9,99,-1,8))
      val equals8Immediate = IntComputer(Memory(3,3,1108,-1,8,3,4,3,99))


      equals8Positional.calculate(Data(8))._2 should be (Data(1))
      equals8Immediate.calculate(Data(8))._2 should be (Data(1))
      for {
        input <- Range(-80, 7).inclusive ++ Range(9, 100)
      } yield {
        equals8Positional.calculate(Data(input))._2 should be(Data(0))
        equals8Immediate.calculate(Data(input))._2 should be(Data(0))
      }

      val lessThan8Positional = IntComputer(Memory(3,9,7,9,10,9,4,9,99,-1,8))
      val lessThan8Immediate = IntComputer(Memory(3,3,1107,-1,8,3,4,3,99))

      for {
        input <- Range(-80, 7).inclusive
      } yield {
        lessThan8Positional.calculate(Data(input))._2 should be(Data(1))
        lessThan8Immediate.calculate(Data(input))._2 should be(Data(1))
      }
      for {
        input <- Range(8, 100)
      } yield {
        lessThan8Positional.calculate(Data(input))._2 should be(Data(0))
        lessThan8Immediate.calculate(Data(input))._2 should be(Data(0))
      }

      val jmpPos = IntComputer(Data(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9))
      val jmpImm = IntComputer(Data(3,3,1105,-1,9,1101,0,0,12,4,12,99,1))

      jmpPos.calculate(Data(0))._2 should be (Data(0))
      jmpImm.calculate(Data(0))._2 should be (Data(0))
      for {
        input <- Range(1, 100)
      } yield {
        jmpPos.calculate(Data(input))._2 should be(Data(1))
        jmpImm.calculate(Data(input))._2 should be(Data(1))
      }

      val compareEight = IntComputer(Data(
        3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99))

      for {
        input <- Range(-80, 7).inclusive
      } yield {
        compareEight.calculate(Data(input))._2 should be(Data(999))
      }
      compareEight.calculate(Data(8))._2 should be(Data(1000))
      for {
        input <- Range(9, 100)
      } yield {
        compareEight.calculate(Data(input))._2 should be(Data(1001))
      }


    }
  }
}
