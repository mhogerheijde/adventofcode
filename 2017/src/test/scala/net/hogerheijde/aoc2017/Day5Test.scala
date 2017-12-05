package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day5.Day5
import net.hogerheijde.aoc2017.day5.Ended
import net.hogerheijde.aoc2017.day5.Running
import net.hogerheijde.aoc2017.day5.State
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day5Test extends WordSpec with Matchers {

  "Day 5" should {
    "calculate next state" in {
      val input = State(IndexedSeq(0, 3, 0, 1, -3))
      val expect1 = Running(IndexedSeq(1, 3, 0, 1, -3), 0, 1)
      val expect2 = Running(IndexedSeq(2, 3, 0, 1, -3), 1, 2)

      val expect3 = Running(IndexedSeq(2, 4, 0, 1, -3), 4, 3)
      val expect4 = Running(IndexedSeq(2, 4, 0, 1, -2), 1, 4)
      val expect5 = Ended(5) //Running(IndexedSeq(2, 5, 0, 1, -2), 5, 5)


      Day5.nextState(input) should be(expect1)
      Day5.nextState(expect1) should be(expect2)
      Day5.nextState(expect2) should be(expect3)
      Day5.nextState(expect3) should be(expect4)
      Day5.nextState(expect4) should be(expect5)
    }

    "resolve state" in {
      val input = State(IndexedSeq(0, 3, 0, 1, -3))
      val expect = Ended(5)

      Day5.resolve(input) should be (expect)

    }

    "resolve state part 2" in {
      val input = State(IndexedSeq(0, 3, 0, 1, -3))
      val expect = Ended(10)

      Day5.resolve2(input) should be (expect)

    }
  }

}
