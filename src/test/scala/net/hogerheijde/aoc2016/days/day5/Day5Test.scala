package net.hogerheijde.aoc2016.days.day5

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day5Test extends FlatSpec with Matchers {


  behavior of "Day5"

  it should "create the correct code" in {
    Day5.crackP("abc") should be("18f47a30")

  }

  it should "calculate the correct next digit" in {

    Day5.nextDigit("abc", 0)       should be ( ('1', 3231929) )
    Day5.nextDigit("abc", 3231930) should be ( ('8', 5017308) )
    Day5.nextDigit("abc", 5017309) should be ( ('f', 5278568) )

  }

}
