package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2021.Day14.Pair
import net.hogerheijde.aoc2021.Day14.Polymer
import net.hogerheijde.aoc2021.Day14.template
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day14Test extends AnyWordSpec with Matchers {

  val exampleInput1 = Day14.parse(
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C
      |""".stripMargin
  )

  "Parser" should {
    "parse example" in {
      exampleInput1 should be (
        Polymer(
          ('N', 'B'),
          Map(
            ('N', 'N') -> 1,
            ('N', 'C') -> 1,
            ('C', 'B') -> 1,
          ),
          Map(
            ('C', 'H') -> Set('C' -> 'B', 'B' -> 'H'),
            ('H', 'H') -> Set('H' -> 'N', 'N' -> 'H'),
            ('C', 'B') -> Set('C' -> 'H', 'H' -> 'B'),
            ('N', 'H') -> Set('N' -> 'C', 'C' -> 'H'),
            ('H', 'B') -> Set('H' -> 'C', 'C' -> 'B'),
            ('H', 'C') -> Set('H' -> 'B', 'B' -> 'C'),
            ('H', 'N') -> Set('H' -> 'C', 'C' -> 'N'),
            ('N', 'N') -> Set('N' -> 'C', 'C' -> 'N'),
            ('B', 'H') -> Set('B' -> 'H', 'H' -> 'H'),
            ('N', 'C') -> Set('N' -> 'B', 'B' -> 'C'),
            ('N', 'B') -> Set('N' -> 'B', 'B' -> 'B'),
            ('B', 'N') -> Set('B' -> 'B', 'B' -> 'N'),
            ('B', 'B') -> Set('B' -> 'N', 'N' -> 'B'),
            ('B', 'C') -> Set('B' -> 'B', 'B' -> 'C'),
            ('C', 'C') -> Set('C' -> 'N', 'N' -> 'C'),
            ('C', 'N') -> Set('C' -> 'C', 'C' -> 'N'),
          ),
        )
      )
    }
  }

  "Model" should {
    "have correct count of elements" in {
      exampleInput1.elementCount should be (
        Map(
          ('B') -> 1,
          ('C') -> 1,
          ('N') -> 2,
        )
      )
    }


    "expand in step 1" in {
      println(histToString(exampleInput1.next.histogram))
      exampleInput1.next.histogram should be (
        histogramFromString("NCNBCHB")
      )
    }
    "expand in step 2" in {
      println(histToString(exampleInput1.next(2).histogram))
      exampleInput1.next(2).histogram should be (
        histogramFromString("NBCCNBBBCBHCB")
      )
    }
    "expand in step 3" in {
      println("Calculated")
      println(histToString(exampleInput1.next(3).histogram))
      println("Expected")
      println(histToString(histogramFromString("NBBBCNCCNBBNBNBBCHBHHBCHB")))
      exampleInput1.next(3).histogram should be (
        histogramFromString("NBBBCNCCNBBNBNBBCHBHHBCHB")
      )
    }
    "expand in step 4" in {
      exampleInput1.next(4).histogram should be (
        histogramFromString("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
      )
    }

    "have correct counts of elements after 10 steps" in {
      val hist = exampleInput1.next(10).elementCount

      hist('H') should be (161)
      hist('B') should be (1749)
    }
  }

  "Day 14" should {
    "calculate part 1" in {
      Day14.part1(exampleInput1) should be (1588)
    }
    "calculate part 2" in {
      Day14.part2(exampleInput1) should be(2188189693529L)
    }
  }

  def histogramFromString(s: String): Map[Pair, Long] = {
    Parser.parse(template(_))(s + "\n").get._1
  }

  def histToString(h: Map[Pair, Long]) = {
    val sb = new StringBuilder
    h.keys.toSeq.sorted.foreach { pair =>
      sb.append(pair._1)
      sb.append(pair._2)
      sb.append(" -> ")
      sb.append(h(pair).toString)
      sb.append("\n")
    }
    sb.toString()
  }

}
