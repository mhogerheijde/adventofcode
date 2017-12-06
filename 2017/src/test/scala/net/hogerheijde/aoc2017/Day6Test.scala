package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day6.Day6
import net.hogerheijde.aoc2017.day6.State
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day6Test extends WordSpec with Matchers {

  "Day6" should {
    "find register of interest" in {
      Day6.max(State.from(0, 2, 7, 0)) should be ( (2, 7) )
      // Break tie by choosing lowest register
      Day6.max(State.from(0, 7, 7, 0)) should be ( (1, 7) )
    }

    "calculate next state" in {
      val state1 = State.from(0, 2, 7, 0)
      val state2 = State.from(2, 4, 1, 2) // 7 distributed over the registers gives 7 / 4 = 1 and 7 % 4 = 3. So all but register 3 get 2 blocks
      val state3 = State.from(3, 1, 2, 3) // 4 distributed over the registers gives 4 / 4 = 1 and 4 % 4 = 0. So all but register 4 get 1 blocks
      val state4 = State.from(0, 2, 3, 4)
      val state5 = State.from(1, 3, 4, 1)
      val state6 = State.from(2, 4, 1, 2)
      Day6.nextState1(state1) should be (state2)
      Day6.nextState1(state2) should be (state3)
      Day6.nextState1(state3) should be (state4)
      Day6.nextState1(state4) should be (state5)
      Day6.nextState1(state5) should be (state6)
    }

    "calculate next state of real input" in {
      val state1 = State.from(2,  8,  8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14)
      val state2 = State.from(3,  9,  9, 6, 5, 3, 4, 2, 6, 6, 2, 3,  0, 14, 6, 15)
      val state3 = State.from(4, 10, 10, 7, 6, 4, 5, 3, 7, 7, 3, 4,  1, 15, 7,  0)
      val state4 = State.from(5, 11, 11, 8, 7, 5, 6, 4, 8, 8, 4, 5,  2,  0, 8,  1)
      val state5 = State.from(5,  0, 12, 9, 8, 6, 7, 5, 9, 9, 5, 6,  3,  0, 8,  1)


      Day6.nextState1(state1) should be(state2)
      Day6.nextState1(state2) should be(state3)
      Day6.nextState1(state3) should be(state4)
      Day6.nextState1(state4) should be(state5)
    }

    "resolve state" in {

      val state = State.from(0, 2, 7, 0)

      val expectedStates = IndexedSeq(
        State.from(0, 2, 7, 0),
        State.from(2, 4, 1, 2),
        State.from(3, 1, 2, 3),
        State.from(0, 2, 3, 4),
        State.from(1, 3, 4, 1)
      )

      val result = Day6.resolve(0, IndexedSeq(), state)

      result._1 should be (5)
      result._2 should be (expectedStates)
      result._3 should be (State.from(2, 4, 1, 2))

    }

    "resolve state" in {
      val state = State.from(0, 2, 7, 0)
      Day6.part2(state) should be (4)

    }


  }

}
