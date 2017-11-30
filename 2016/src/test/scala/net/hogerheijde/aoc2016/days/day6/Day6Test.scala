package net.hogerheijde.aoc2016.days.day6

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.IndexedSeq

class Day6Test extends FlatSpec with Matchers {
  val sampleInput = """eedadn
                |drvtee
                |eandsr
                |raavrd
                |atevrs
                |tsrnev
                |sdttsa
                |rasrtv
                |nssdts
                |ntnada
                |svetve
                |tesnvt
                |vntsnd
                |vrdear
                |dvrsen
                |enarar""".stripMargin


  "Day6" should "create a correct histogram" in {
    Day6.histogram(IndexedSeq("abcdefgh", "hgfedcba")) should be(
      IndexedSeq(
        Map('a' -> 1, 'h' -> 1),
        Map('b' -> 1, 'g' -> 1),
        Map('c' -> 1, 'f' -> 1),
        Map('d' -> 1, 'e' -> 1),
        Map('e' -> 1, 'd' -> 1),
        Map('f' -> 1, 'c' -> 1),
        Map('g' -> 1, 'b' -> 1),
        Map('h' -> 1, 'a' -> 1)
      )
    )

    Day6.histogram(IndexedSeq("hello", "helli")) should be(
      IndexedSeq(
        Map('h' -> 2),
        Map('e' -> 2),
        Map('l' -> 2),
        Map('l' -> 2),
        Map('o' -> 1, 'i' -> 1)
      )
    )
  }

  it should "error correct a histogram" in {
    val input = IndexedSeq(
      Map('h' -> 3),
      Map('e' -> 3),
      Map('l' -> 3),
      Map('l' -> 3),
      Map('o' -> 2, 'i' -> 1)
    )


    Day6.errorCorrect(input) should be ("hello")

  }

  it should "return an error corrected message from input" in {
    val input = """hello
      |helli
      |iello
      |""".stripMargin


    Day6.process(input) should be ("hello")
  }

  it should "return easter for sample input" in {
    Day6.process(sampleInput) should be ("easter")
  }

  it should "return advent for sample input with pt2 algorithm" in {
    Day6.process2(sampleInput) should be ("advent")
  }

}
