package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day7.Day7
import net.hogerheijde.aoc2017.day7.Disc
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day7Test extends WordSpec with Matchers {

  val inputSmall =
    """
      |xhth (51)
      |ktlj (52)
      |fwft (72) -> ktlj, cntj, xhth
      |cntj (53)
    """.stripMargin.trim

  val input =
    """
      |pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
    """.stripMargin.trim


  "Day7" should {
    "parse input" in {

      val expect = Map(
        Disc("xhth", 51) -> IndexedSeq(),
        Disc("ktlj", 52) -> IndexedSeq(),
        Disc("fwft", 72) -> IndexedSeq(
          Disc("ktlj", 52),
          Disc("cntj", 53),
          Disc("xhth", 51)
        ),
        Disc("cntj", 53) -> IndexedSeq()
      )
      Day7.parse(inputSmall) should be(expect)
    }


    "find root" in {
      Day7.part1(Day7.parse(inputSmall)) should be ("fwft")
      Day7.part1(Day7.parse(input)) should be ("tknk")
    }


  }

}
