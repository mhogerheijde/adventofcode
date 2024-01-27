package net.hogerheijde.aoc.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// scalastyle:off magic.number

class CircularBufferTest extends AnyWordSpec with Matchers {


  "Empty CircularBuffer" should {
    "have no current" in {
      CircularBuffer().headOption should be(None)
    }

    "rotate" in {
      CircularBuffer().rotate should be(CircularBuffer())
      (-10 to 10).foreach (i => CircularBuffer().rotate(i) should be(CircularBuffer()))
    }

    "equals other empty CicrularBuffer" in {
      CircularBuffer(Seq.empty[String]:_*) should be (CircularBuffer(Seq.empty[String]:_*))
      CircularBuffer(Seq.empty[Int]:_*) should be (CircularBuffer(Seq.empty[Int]:_*))
    }

    "should not equal other empty CicrularBuffer of different type" ignore {
      CircularBuffer(Seq.empty[String]: _*) shouldNot be(CircularBuffer(Seq.empty[Int]: _*))
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
      CircularBuffer(1, 2, 3, 4).rotate should be(CircularBuffer(2, 3, 4, 1))
      CircularBuffer(1, 2, 3, 4).rotate(1) should be(CircularBuffer(2, 3, 4, 1))
    }

    "rotate twice" in {
      CircularBuffer(1, 2, 3, 4).rotate.rotate should be(CircularBuffer(3, 4, 1, 2))

      CircularBuffer(1, 2, 3, 4).rotate(2) should be(CircularBuffer(3, 4, 1, 2))
      CircularBuffer(1, 2, 3, 4).rotate(6) should be(CircularBuffer(3, 4, 1, 2))

      CircularBuffer(1, 2, 3, 4).rotate(3) should be(CircularBuffer(4, 1, 2, 3))
      CircularBuffer(1, 2, 3, 4).rotate(7) should be(CircularBuffer(4, 1, 2, 3))

      CircularBuffer(1, 2, 3, 4).rotate(4) should be(CircularBuffer(1, 2, 3, 4))
      CircularBuffer(1, 2, 3, 4).rotate(8) should be(CircularBuffer(1, 2, 3, 4))
    }

    "rotate backwards" in {
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-1) should be(CircularBuffer(6, 1, 2, 3, 4, 5))
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-7) should be(CircularBuffer(6, 1, 2, 3, 4, 5))

      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-2) should be(CircularBuffer(5, 6, 1, 2, 3, 4))
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-8) should be(CircularBuffer(5, 6, 1, 2, 3, 4))

      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-3) should be(CircularBuffer(4, 5, 6, 1, 2, 3))
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-9) should be(CircularBuffer(4, 5, 6, 1, 2, 3))

      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-4) should be(CircularBuffer(3, 4, 5, 6, 1, 2))
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-10) should be(CircularBuffer(3, 4, 5, 6, 1, 2))

      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-6) should be(CircularBuffer(1, 2, 3, 4, 5, 6))
      CircularBuffer(1, 2, 3, 4, 5, 6).rotate(-12) should be(CircularBuffer(1, 2, 3, 4, 5, 6))
    }

  }

}
