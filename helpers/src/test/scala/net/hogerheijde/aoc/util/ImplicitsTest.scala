package net.hogerheijde.aoc.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import net.hogerheijde.aoc.util.Implicits.BooleanHelper

class ImplicitsTest extends AnyWordSpec with Matchers {


  "BooleanHelper" should {
    "calculate xor" in {
      (false xor false) should be (false)
      (false xor true) should be (true)
      (true xor false) should be (true)
      (true xor true) should be (false)
    }
  }

}
