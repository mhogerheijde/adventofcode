package net.hogerheijde.aoc2016.days.day2.model

import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.One
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Two
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Three
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Four
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Five
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Six
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Seven
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Eight
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare.Nine

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class KeyTest extends FlatSpec with Matchers {

  "Key One" should "update correctly" in {
    One.update(GoLeft)  should be (One)
    One.update(GoRight) should be (Two)
    One.update(GoUp)    should be (One)
    One.update(GoDown)  should be (Four)
  }

  "Key Two" should "update correctly" in {
    Two.update(GoLeft)  should be (One)
    Two.update(GoRight) should be (Three)
    Two.update(GoUp)    should be (Two)
    Two.update(GoDown)  should be (Five)
  }

  "Key Three" should "update correctly" in {
    Three.update(GoLeft)  should be (Two)
    Three.update(GoRight) should be (Three)
    Three.update(GoUp)    should be (Three)
    Three.update(GoDown)  should be (Six)
  }

  "Key Four" should "update correctly" in {
    Four.update(GoLeft)  should be (Four)
    Four.update(GoRight) should be (Five)
    Four.update(GoUp)    should be (One)
    Four.update(GoDown)  should be (Seven)
  }

  "Key Five" should "update correctly" in {
    Five.update(GoLeft)  should be (Four)
    Five.update(GoRight) should be (Six)
    Five.update(GoUp)    should be (Two)
    Five.update(GoDown)  should be (Eight)
  }

  "Key Six" should "update correctly" in {
    Six.update(GoLeft)  should be (Five)
    Six.update(GoRight) should be (Six)
    Six.update(GoUp)    should be (Three)
    Six.update(GoDown)  should be (Nine)
  }

  "Key Seven" should "update correctly" in {
    Seven.update(GoLeft)  should be (Seven)
    Seven.update(GoRight) should be (Eight)
    Seven.update(GoUp)    should be (Four)
    Seven.update(GoDown)  should be (Seven)
  }

  "Key Eight" should "update correctly" in {
    Eight.update(GoLeft)  should be (Seven)
    Eight.update(GoRight) should be (Nine)
    Eight.update(GoUp)    should be (Five)
    Eight.update(GoDown)  should be (Eight)
  }

  "Key Nine" should "update correctly" in {
    Nine.update(GoLeft)  should be (Eight)
    Nine.update(GoRight) should be (Nine)
    Nine.update(GoUp)    should be (Six)
    Nine.update(GoDown)  should be (Nine)
  }
}
