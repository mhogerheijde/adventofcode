package net.hogerheijde.aoc2015

import net.hogerheijde.aoc2015.day5.Day5
import net.hogerheijde.aoc2015.day5.Naughty
import net.hogerheijde.aoc2015.day5.Nice
import org.scalatest.Matchers
import org.scalatest.WordSpec

class TestDay5 extends WordSpec with Matchers {

  "Day5" should {
    "find doubles" in {
      Day5.containsDoubles("abcde") should be(false)
      Day5.containsDoubles("abcb") should be(false)
      Day5.containsDoubles("aabc") should be(true)
      Day5.containsDoubles("abbcde") should be(true)
      Day5.containsDoubles("abccde") should be(true)
      Day5.containsDoubles("aabccde") should be(true)
      Day5.containsDoubles("abcdee") should be(true)
    }

    "check if enough vowels" in {
      Day5.containsAtLeast3Vowels("ugknbfddgicrmopn") should be (true)
      Day5.containsAtLeast3Vowels("aaa") should be (true)
      Day5.containsAtLeast3Vowels("dvszwmarrgswjxm") should be (false)
    }

    "check if illegal combinations are not found" in {
      Day5.containsNoIllegalCombinations("ugknbfddgicrmopn") should be (true)
      Day5.containsNoIllegalCombinations("aaa") should be (true)
      Day5.containsNoIllegalCombinations("abd") should be (false)
      Day5.containsNoIllegalCombinations("bcpqd") should be (false)
      Day5.containsNoIllegalCombinations("bcd") should be (false)
      Day5.containsNoIllegalCombinations("asdfxy") should be (false)

    }


    "check if words are Nice" in {

      Day5.judge("ugknbfddgicrmopn") should be (Nice)
      Day5.judge("aaa") should be (Nice)
      Day5.judge("jchzalrnumimnmhp") should be (Naughty)
      Day5.judge("haegwjzuvuyypxyu") should be (Naughty)
      Day5.judge("dvszwmarrgswjxmb") should be (Naughty)

    }


  }

}
