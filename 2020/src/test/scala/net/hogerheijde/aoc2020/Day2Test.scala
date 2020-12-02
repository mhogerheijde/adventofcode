package net.hogerheijde.aoc2020

import net.hogerheijde.aoc2020.Day2.PasswordRule
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with Matchers {

  "Day 2 - part 1" should {

    "recognise valid passwords" in {
      Day2.isValidDay1(PasswordRule(1, 3, 'a', "abcde")) should be (true)
      Day2.isValidDay1(PasswordRule(2, 9, 'c', "ccccccccc")) should be (true)
    }

    "recognise invalid passwords" in {
      Day2.isValidDay1(PasswordRule(1, 3, 'b', "cdefg")) should be (false)
    }
  }


  "Day 2 - part 2" should {

    "recognise valid passwords" in {
      Day2.isValidDay2(PasswordRule(1, 3, 'a', "abcde")) should be (true)
    }

    "recognise invalid passwords" in {
      Day2.isValidDay2(PasswordRule(2, 9, 'c', "ccccccccc")) should be (false)
      Day2.isValidDay2(PasswordRule(1, 3, 'b', "cdefg")) should be (false)
    }
  }
}
