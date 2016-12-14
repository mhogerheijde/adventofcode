package net.hogerheijde.aoc2016.days.day4

import net.hogerheijde.aoc2016.days.day4.Day4.Histogram
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day4Test extends FlatSpec with Matchers {

  "Day4" should "build a correct histogram" in {

    Day4.histogram("abbcccddddeeeee") should be (Histogram(
      'a' -> 1,
      'b' -> 2,
      'c' -> 3,
      'd' -> 4,
      'e' -> 5
    ))

    Day4.histogram("akeufgjkwosdkuatlknavi") should be (Histogram(
      'a' -> 3,
      'k' -> 4,
      'e' -> 1,
      'u' -> 2,
      'f' -> 1,
      'g' -> 1,
      'j' -> 1,
      'w' -> 1,
      'o' -> 1,
      's' -> 1,
      'd' -> 1,
      't' -> 1,
      'l' -> 1,
      'n' -> 1,
      'v' -> 1,
      'i' -> 1
    ))
  }

  it should "split input into tuple correctly" in  {
    Day4.parse("fubrjhqlf-edvnhw-dftxlvlwlrq-803[wjvzd]") should be( ("fubrjhqlfedvnhwdftxlvlwlrq", 803, "wjvzd") )
    Day4.parse("kzgwomvqk-rmttgjmiv-lmxizbumvb-902[zmnji]") should be( ("kzgwomvqkrmttgjmivlmxizbumvb", 902, "zmnji") )
    Day4.parse("dkqjcbctfqwu-dwppa-fgukip-596[syiua]") should be( ("dkqjcbctfqwudwppafgukip", 596, "syiua") )
    Day4.parse("xjinphzm-bmvyz-ytz-gjbdnodxn-135[nzbdj]") should be( ("xjinphzmbmvyzytzgjbdnodxn", 135, "nzbdj") )
    Day4.parse("uwtojhynqj-hfsid-xytwflj-177[ztsqu]") should be( ("uwtojhynqjhfsidxytwflj", 177, "ztsqu") )
  }

  it should "create a checksum" in {
    Day4.createChecksum(Day4.histogram("qqqqqbbbzyx")) should be("qbxyz")
    Day4.createChecksum(Day4.histogram("abcdefgh")) should be("abcdefgh")
    Day4.createChecksum(Day4.histogram("notarealroom")) should be("oarelmnt")
  }

  it should "filter invalid rooms" in {
    Day4.correctChecksum(("aaaaabbbzyx", 123, "abxyz")) should be(true)
    Day4.correctChecksum(("abcdefgh", 987, "abcde")) should be(true)
    Day4.correctChecksum(("notarealroom", 404, "oarel")) should be(true)
    Day4.correctChecksum(("totallyrealroom", 200, "decoy")) should be(false)

  }

}
