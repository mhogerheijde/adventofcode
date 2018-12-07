package net.hogerheijde.aoc2018.day5

import org.scalatest.Matchers
import org.scalatest.WordSpec

class ReactorTest extends WordSpec with Matchers {

  "Reactor" should {
    "test for type" in {
      Reactor.isSameType('a', "a") should be(true)
      Reactor.isSameType('a', "A") should be(true)
      Reactor.isSameType('A', "a") should be(true)
      Reactor.isSameType('A', "A") should be(true)

      Reactor.isSameType('a', "b") should be(false)
      Reactor.isSameType('a', "B") should be(false)
      Reactor.isSameType('A', "b") should be(false)
      Reactor.isSameType('A', "B") should be(false)

      Reactor.isSameType('a', "") should be(false)
      Reactor.isSameType('A', "") should be(false)
    }

    "test polarity" in {
      Reactor.haveDifferentPolarity('a', "A") should be(true)
      Reactor.haveDifferentPolarity('A', "a") should be(true)
      Reactor.haveDifferentPolarity('b', "A") should be(true)
      Reactor.haveDifferentPolarity('B', "a") should be(true)


      Reactor.haveDifferentPolarity('a', "a") should be(false)
      Reactor.haveDifferentPolarity('A', "A") should be(false)
      Reactor.haveDifferentPolarity('b', "a") should be(false)
      Reactor.haveDifferentPolarity('B', "A") should be(false)

      Reactor.haveDifferentPolarity('a', "") should be(false)
      Reactor.haveDifferentPolarity('B', "") should be(false)
    }


    "test reaction" in {
      Reactor.reactsWith('a', "A") should be (true)
      Reactor.reactsWith('A', "a") should be (true)

      Reactor.reactsWith('a', "a") should be (false)
      Reactor.reactsWith('A', "A") should be (false)
      Reactor.reactsWith('a', "B") should be (false)
      Reactor.reactsWith('A', "b") should be (false)
    }

    "reduce input" in {
      Reactor.reduce("dabAcCaCBAcCcaDA") should be("dabCBAcaDA")
    }

    "reduce input with filter" in {
      Reactor.reduceFiltered("dabAcCaCBAcCcaDA", 'a') should be("dbCBcD")
      Reactor.reduceFiltered("dabAcCaCBAcCcaDA", 'b') should be("daCAcaDA")
      Reactor.reduceFiltered("dabAcCaCBAcCcaDA", 'c') should be("daDA")
      Reactor.reduceFiltered("dabAcCaCBAcCcaDA", 'd') should be("abCBAc")
    }
  }

}
