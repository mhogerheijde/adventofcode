package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day12.Day12
import net.hogerheijde.aoc2017.day13.ActiveLayer
import net.hogerheijde.aoc2017.day13.Day13
import net.hogerheijde.aoc2017.day13.Firewall
import net.hogerheijde.aoc2017.day13.InactiveLayer
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day13Test  extends WordSpec with Matchers {

  val input =
    """0: 3
      |1: 2
      |4: 4
      |6: 4""".stripMargin

  val exampleFirewall = Firewall(Map(
     0 -> ActiveLayer(3),
     1 -> ActiveLayer(2),
     4 -> ActiveLayer(4),
     6 -> ActiveLayer(4),
   ))


  "Firewall" should {
    "have InactiveLayer at spots where no layer is found" in {
      Firewall(Map()).layer(0) should be(InactiveLayer)
    }

    "catch packet in picosecond 0 and 6" in  {
      exampleFirewall.detect(0) should be(true)
      exampleFirewall.detect(1) should be(false)
      exampleFirewall.detect(2) should be(false)
      exampleFirewall.detect(3) should be(false)
      exampleFirewall.detect(4) should be(false)
      exampleFirewall.detect(5) should be(false)
      exampleFirewall.detect(6) should be(true)
    }

    "have severity" in {
      exampleFirewall.severity == 24
    }
  }


   "Day 13" should {
     "parse" in {
       Day13.parse(input) should be(
         Firewall(Map(
           0 -> ActiveLayer(3),
           1 -> ActiveLayer(2),
           4 -> ActiveLayer(4),
           6 -> ActiveLayer(4)
         ))
       )
     }
   }

}
