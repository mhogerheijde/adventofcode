package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day10.Ended
import net.hogerheijde.aoc2017.day10.Running
import net.hogerheijde.aoc2017.day10.State
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq


class Day10Test extends WordSpec with Matchers {

  val exampleInput = Running(IndexedSeq(0, 1, 2, 3, 4), 0, 0, IndexedSeq(3, 4, 1, 5))

  "Day 10" should {
    "calculate next state" in {

      val expected1 = Running(IndexedSeq(3, 4, 2, 1, 0), 3, 1, IndexedSeq(4, 1, 5))
      val expected2 = Running(IndexedSeq(1, 2, 4, 3, 0), 3, 2, IndexedSeq(1, 5))
      val expected3 = Running(IndexedSeq(3, 0, 1, 2, 4), 1, 3, IndexedSeq(5))
      val expected4 = Ended(IndexedSeq(3, 4, 2, 1, 0))

      exampleInput.next should be(expected1)
      expected1.next should be(expected2)
      expected2.next should be(expected3)
      expected3.next should be(expected4)


    }

    "print stuff" in {
      Running(IndexedSeq(0, 1, 2, 3, 4), 0, 0, IndexedSeq(1)).toString should be("Running([0] 1 2 3 4 | 0 | 1)")
      Running(IndexedSeq(1, 2, 3, 4, 0), 1, 0, IndexedSeq(1)).toString should be("Running(0 [1] 2 3 4 | 0 | 1)")
      Running(IndexedSeq(2, 3, 4, 0, 1), 2, 0, IndexedSeq(1)).toString should be("Running(0 1 [2] 3 4 | 0 | 1)")
      Running(IndexedSeq(3, 4, 0, 1, 2), 3, 0, IndexedSeq(1)).toString should be("Running(0 1 2 [3] 4 | 0 | 1)")
    }
  }
  "State" should {
    "rotate" in {
      State.rotate(IndexedSeq(0, 1, 2, 3, 4), 0) should be (IndexedSeq(0, 1, 2, 3, 4))
      State.rotate(IndexedSeq(0, 1, 2, 3, 4), 1) should be (IndexedSeq(1, 2, 3, 4, 0))
      State.rotate(IndexedSeq(0, 1, 2, 3, 4), 2) should be (IndexedSeq(2, 3, 4, 0, 1))
      State.rotate(IndexedSeq(0, 1, 2, 3, 4), -1) should be (IndexedSeq(4, 0, 1, 2, 3))
      State.rotate(IndexedSeq(0, 1, 2, 3, 4), -2) should be (IndexedSeq(3, 4, 0, 1, 2))


    }

  }

}
