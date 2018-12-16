package net.hogerheijde.aoc.common.parser

import net.hogerheijde.aoc.util.Parser
import org.scalatest.Matchers
import org.scalatest.WordSpec

class CommonTest extends WordSpec with Matchers {

  "int parser" should {
    "parse positive ints" in {
      Parser.parse(Common.int(_))("1") should contain (1)
      Parser.parse(Common.int(_))("12") should contain (12)
      Parser.parse(Common.int(_))("123") should contain (123)
      Parser.parse(Common.int(_))("023") should contain (23)
    }

    "parse negative ints" in {
      Parser.parse(Common.int(_))("-1") should contain (-1)
      Parser.parse(Common.int(_))("-12") should contain (-12)
      Parser.parse(Common.int(_))("-123") should contain (-123)
      Parser.parse(Common.int(_))("-023") should contain (-23)
    }
  }

  "intSeq parser" should {
    "parse ints separated by space" in {
      Parser.parse(Common.intSeq(_))("1") should contain (IndexedSeq(1))
      Parser.parse(Common.intSeq(_))("1 ") should contain (IndexedSeq(1))
      Parser.parse(Common.intSeq(_))("1 12") should contain (IndexedSeq(1, 12))
      Parser.parse(Common.intSeq(_))("1  -12  123") should contain (IndexedSeq(1, -12, 123))
    }

    "parse ints separated by comma" in {
      Parser.parse(Common.intSeq(_))("1") should contain (IndexedSeq(1))
      Parser.parse(Common.intSeq(_))("1 ") should contain (IndexedSeq(1))
      Parser.parse(Common.intSeq(_))("1, 12") should contain (IndexedSeq(1, 12))
      Parser.parse(Common.intSeq(_))("1,  -12,  123") should contain (IndexedSeq(1, -12, 123))
    }
  }

}
