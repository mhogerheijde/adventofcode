package net.hogerheijde.aoc2016.days.day7

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.immutable.IndexedSeq

class Day7Test extends FlatSpec with Matchers {

  "Day7" should "recognise test input" in {

    Day7.supportsTLS(Day7.IP(IndexedSeq("abba", "qrst"), IndexedSeq("mnop"))) should be(true)
    Day7.supportsTLS(Day7.IP(IndexedSeq("ioxxoj", "zxcvbn"), IndexedSeq("asdfgh"))) should be(true)

    Day7.supportsTLS(Day7.IP(IndexedSeq("abcd", "xyyx"), IndexedSeq("bddb"))) should be(false)
    Day7.supportsTLS(Day7.IP(IndexedSeq("aaaa", "tyui"), IndexedSeq("qwer"))) should be(false)

  }

  it should "recognise pattern" in {
    Day7.containsPattern("abba") should be(true)
    Day7.containsPattern("ioxxoj") should be(true)
    Day7.containsPattern("bddb") should be(true)
    Day7.containsPattern("aaaa") should be(false)
  }

  it should "parse IPs from string" in {
    Day7.parseIp("abba[mnop]qrst") should be ((IndexedSeq("abba", "qrst"), IndexedSeq("mnop")))
    Day7.parseIp("abcd[bddb]xyyx") should be ((IndexedSeq("abcd", "xyyx"), IndexedSeq("bddb")))
    Day7.parseIp("aaaa[qwer]tyui") should be ((IndexedSeq("aaaa", "tyui"), IndexedSeq("qwer")))
    Day7.parseIp("ioxxoj[asdfgh]zxcvbn") should be((IndexedSeq("ioxxoj", "zxcvbn"), IndexedSeq("asdfgh")))
  }

  it should "build correct anti-patterns" in {
    val input = IndexedSeq("aba", "xyx", "aaa", "zazbz")
    Day7.antiPatterns(input) should be(IndexedSeq("bab", "yxy", "aza", "bzb"))
  }

  it should "detect SSL correctly" in {
    Day7.supportsSSL(Day7.IP(IndexedSeq("aba", "xyz"), IndexedSeq("bab"))) should be (true)
    Day7.supportsSSL(Day7.IP(IndexedSeq("aaa", "eke"), IndexedSeq("kek") )) should be (true)
    Day7.supportsSSL(Day7.IP(IndexedSeq("zazbz", "cdb"), IndexedSeq("bzb") )) should be (true)
    Day7.supportsSSL(Day7.IP(IndexedSeq("xyx", "xyx"), IndexedSeq("xyx") )) should be (false)
  }

}
