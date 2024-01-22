package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day7.Card.A
import net.hogerheijde.aoc2023.Day7.Card.K
import net.hogerheijde.aoc2023.Day7.Card.Q
import net.hogerheijde.aoc2023.Day7.Card.J
import net.hogerheijde.aoc2023.Day7.Card.K
import net.hogerheijde.aoc2023.Day7.Card.K
import net.hogerheijde.aoc2023.Day7.Card.T
import net.hogerheijde.aoc2023.Day7.Card.T
import net.hogerheijde.aoc2023.Day7.Card._9
import net.hogerheijde.aoc2023.Day7.Card._8
import net.hogerheijde.aoc2023.Day7.Card._7
import net.hogerheijde.aoc2023.Day7.Card._6
import net.hogerheijde.aoc2023.Day7.Card._5
import net.hogerheijde.aoc2023.Day7.Card._4
import net.hogerheijde.aoc2023.Day7.Card._3
import net.hogerheijde.aoc2023.Day7.Card._2
import net.hogerheijde.aoc2023.Day7.Hand
import net.hogerheijde.aoc2023.Day7.Type.FiveOfAKind
import net.hogerheijde.aoc2023.Day7.Type.FiveOfAKind
import net.hogerheijde.aoc2023.Day7.Type.FourOfAKind
import net.hogerheijde.aoc2023.Day7.Type.FullHouse
import net.hogerheijde.aoc2023.Day7.Type.ThreeOfAKind
import net.hogerheijde.aoc2023.Day7.Type.TwoPair
import net.hogerheijde.aoc2023.Day7.Type.OnePair
import net.hogerheijde.aoc2023.Day7.Type.HighCard
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class Day7Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".stripMargin

  val exampleModel: Day7.Model = Seq(
    (Hand(Seq(_3, _2, T, _3, K)), 765),
    (Hand(Seq(T, _5, _5, J, _5)), 684),
    (Hand(Seq(K, K, _6, _7, _7)), 28),
    (Hand(Seq(K, T, J, J, T)), 220),
    (Hand(Seq(Q, Q, Q, J, A)), 483),
  )

  "Day 7 parser" should {
    "parse" in {
      // Parser.parse(...)("...") should be ("")
    }
  }

  "Cards" should {
    "have order" in {
      val ordered = Seq(_2, _3, _4, _5, _6, _7, _8, _9, T, J, Q, K, A)
      (1 to 100).foreach { _ =>
        Random.shuffle(ordered).sorted should be(ordered)
      }
    }
  }
  "Types" should {
    "have order" in {
      val ordered = Seq(HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind)
      (1 to 100).foreach { _ =>
        Random.shuffle(ordered).sorted should be(ordered)
      }
    }
  }

  "Hand" should {
    "make histogram" in {
      Hand.hist(Seq(A, A, A, A, A)) should be(Map(5 -> Set(A)))
      Hand.hist(Seq(A, A, A, A, K)) should be(Map(4 -> Set(A), 1 -> Set(K)))
      Hand.hist(Seq(A, A, A, K, K)) should be(Map(3 -> Set(A), 2 -> Set(K)))
      Hand.hist(Seq(A, A, A, K, T)) should be(Map(3 -> Set(A), 1 -> Set(K, T)))
      Hand.hist(Seq(A, A, K, K, T)) should be(Map(2 -> Set(A, K), 1 -> Set(T)))
      Hand.hist(Seq(A, A, K, Q, T)) should be(Map(2 -> Set(A), 1 -> Set(K, Q, T)))
      Hand.hist(Seq(A, K, Q, J, T)) should be(Map(1 -> Set(A, K, Q, J, T)))
    }

    "recognise its type" in {
      Hand(Seq(A, A, A, A, A)).`type` should be(FiveOfAKind)
      Hand(Seq(A, A, A, A, K)).`type` should be(FourOfAKind)
      Hand(Seq(A, A, A, K, K)).`type` should be(FullHouse)
      Hand(Seq(A, A, A, K, T)).`type` should be(ThreeOfAKind)
      Hand(Seq(A, A, K, K, T)).`type` should be(TwoPair)
      Hand(Seq(A, A, K, Q, T)).`type` should be(OnePair)
      Hand(Seq(A, K, Q, J, T)).`type` should be(HighCard)
    }

    "have order" in {
      val ordered = Seq(
        (Hand(Seq(_3, _2, T, _3, K)), 765),
        (Hand(Seq(K, T, J, J, T)), 220),
        (Hand(Seq(K, K, _6, _7, _7)), 28),
        (Hand(Seq(T, _5, _5, J, _5)), 684),
        (Hand(Seq(Q, Q, Q, J, A)), 483),
      )

      exampleModel.sortBy(_._1)(Day7.part1Ordering) should be (ordered)
    }

  }

  "Day 7" should {

    "parse input" in {
      Day7.parse(exampleInput) should be(exampleModel)
    }

    "Part1: example answer" in {
      Day7.part1(Day7.parse(exampleInput)) should be(6440)
    }

    "Part2: example answer" in {
      Day7.part2(Day7.parse(exampleInput)) should be(0)
    }
  }
}