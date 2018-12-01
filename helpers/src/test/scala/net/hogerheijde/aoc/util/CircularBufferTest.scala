package net.hogerheijde.aoc.util

import org.scalatest.Matchers
import org.scalatest.WordSpec

// scalastyle:off magic.number

class CircularBufferTest extends WordSpec with Matchers {


  "Empty CircularBuffer" should {
    "have no current" in {
      CircularBuffer().currentOption should be(None)
    }

    "rotate" in {
      CircularBuffer().rotate should be(CircularBuffer())
    }

    "equals other empty CicrularBuffer" in {
      CircularBuffer(Seq.empty[String]) should be (CircularBuffer(Seq.empty[String]))
      CircularBuffer(Seq.empty[Int]) should be (CircularBuffer(Seq.empty[Int]))
    }

  }

  "Non empty CircularBuffer" should {

    "equals other CicrularBuffer" in {
      CircularBuffer(1, 2, 3) should be (CircularBuffer(1, 2, 3))
      CircularBuffer("a", "bc", "def") should be (CircularBuffer("a", "bc", "def"))
    }

    "not equal other different CicrularBuffer" in {
      CircularBuffer(1, 2, 3) should not be (CircularBuffer(2, 3, 1))
      CircularBuffer("a", "bc", "def") should not be (CircularBuffer("bc", "def", "a"))
      CircularBuffer("a", "bc", "def") should not be (CircularBuffer(2, 3, 1))
    }

    "not equal empty CircularBuffers" in {
      CircularBuffer(1, 2, 3) should not be (CircularBuffer())
    }

    "rotate once" in {
      CircularBuffer(1, 2, 3, 4).rotate should be(
        CircularBuffer(2, 3, 4, 1)
      )
    }


    "rotate twice" in {
      CircularBuffer(1, 2, 3, 4).rotate.rotate should be(
        CircularBuffer(3, 4, 1, 2)
      )
    }

  }

}
