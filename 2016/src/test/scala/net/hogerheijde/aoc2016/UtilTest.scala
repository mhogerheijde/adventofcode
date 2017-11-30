package net.hogerheijde.aoc2016

import net.hogerheijde.aoc2016.days.day1.model.Coordinate
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class UtilTest extends FlatSpec with Matchers {



  "Util" should "return the first duplicate" in {
    Util.getFirstDupe(IndexedSeq(1, 2, 5, 6, 7, 4, 2, 3, 4, 9, 8)) should be (Some(2))
    Util.getFirstDupe(IndexedSeq(1, 2, 5, 6, 7, 4, 3, 4, 9, 8)) should be (Some(4))
  }

  it should "return the None when there is no duplicate" in {
    val list = IndexedSeq(1, 2, 5, 6, 7, 4, 3, 9, 8)
    Util.getFirstDupe(list) should be (None)
  }


}
