package net.hogerheijde.aoc2018.day14

import scala.collection.immutable.IndexedSeq

import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day14Test extends WordSpec with Matchers {

  "Recipies" should {
    "calculate generations" in {

      val gen0 = Recipies.initialState
      val gen1 = gen0.next
      val gen2 = gen1.next
      val gen3 = gen2.next
      val gen4 = gen3.next
      val gen5 = gen4.next
      val genX = gen5.next(10)


      gen0 should be (Recipies(IndexedSeq(3, 7).reverse, 0, 1))
      gen1 should be (Recipies(IndexedSeq(3, 7, 1, 0).reverse, 0, 1))
      gen2 should be (Recipies(IndexedSeq(3, 7, 1, 0, 1, 0).reverse, 4, 3))
      gen3 should be (Recipies(IndexedSeq(3, 7, 1, 0, 1, 0, 1).reverse, 6, 4))
      gen4 should be (Recipies(IndexedSeq(3, 7, 1, 0, 1, 0, 1, 2).reverse, 0, 6))
      gen5 should be (Recipies(IndexedSeq(3, 7, 1, 0, 1, 0, 1, 2, 4).reverse, 4, 8))

      genX should be (Recipies(IndexedSeq(3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9, 2).reverse, 8, 4))

    }

    "make enough recipies" in {

      Recipies.initialState.makeRecipies(19) should be (
        Recipies(IndexedSeq(3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9).reverse, 6, 2)
      )

    }

    "find winning score" in {
      Recipies.initialState.findPart1Score(9) should be("5158916779")
      Recipies.initialState.findPart1Score(5) should be("0124515891")
      Recipies.initialState.findPart1Score(18) should be("9251071085")
      Recipies.initialState.findPart1Score(2018) should be("5941429882")
    }

    "find part 2 winning score" in {

      Recipies.initialState.findPart2Score("51589") should be (9)
      Recipies.initialState.findPart2Score("01245") should be (5)
      Recipies.initialState.findPart2Score("92510") should be (18)
      Recipies.initialState.findPart2Score("59414") should be (2018)

    }
  }

}
