package net.hogerheijde.aoc2018.day9

import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day9Test extends WordSpec with Matchers {

  "Game" should {
    "solve" in {
      Game.start(9, 25).solve.hiScore should be(32)
      Game.start(10, 1618).solve.hiScore should be(8317)
      Game.start(13, 7999).solve.hiScore should be(146373)
      Game.start(17, 1104).solve.hiScore should be(2764)
      Game.start(21, 6111).solve.hiScore should be(54718)
      Game.start(30, 5807).solve.hiScore should be(37305)
    }
  }

  "Marbles" should {
    "add marble to clean board" in {
      Marbles.cleanBoard.addMarble(1)._1 should be (
        Marbles(IndexedSeq(0, 1), 1)
      )
    }
    "add marbles to board with 2 marbles" in {
      Marbles(IndexedSeq(0, 1), 1).addMarble(2)._1 should be (
        Marbles(IndexedSeq(0, 2, 1), 1)
      )
    }

    "add marbles to board with 3 marbles" in {
      Marbles(IndexedSeq(0, 2, 1), 1).addMarble(3)._1 should be (
        Marbles(IndexedSeq(0, 2, 1, 3), 3)
      )
    }

    "add marbles with many marbles, mid board" in {
      Marbles(IndexedSeq(0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15), 11).addMarble(22)._1 should be(
        Marbles(IndexedSeq(
          0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5,
          22, // Added here!
          11, 1, 12, 6, 13, 3, 14, 7, 15), 13)
      )
    }

    "add marbles with many marbles, at the end" in {
      Marbles(IndexedSeq(0, 4, 2, 5, 1, 6, 3), 5).addMarble(7)._1 should be(
        Marbles(IndexedSeq(0, 4, 2, 5, 1, 6, 3, 7), 7) // added at the end
      )
    }

    "add marbles with many marbles, over the edge" in {
      Marbles(IndexedSeq(0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15), 15).addMarble(16)._1 should be(
        Marbles(IndexedSeq(
          0,
          16, // added here!
          8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15
          ), 1)
      )
    }

    "calculate score" in {
      Marbles(IndexedSeq(0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 13).addMarble(23) should be(
        (
          Marbles(IndexedSeq(0, 16, 8, 17, 4, 18,
            // 9 removed
            19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 6),
          32 // The score
        )
      )
    }

  }

}
