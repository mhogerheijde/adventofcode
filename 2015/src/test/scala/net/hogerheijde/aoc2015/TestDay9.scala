package net.hogerheijde.aoc2015

import net.hogerheijde.aoc.util.Parser
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class TestDay9 extends AnyWordSpec with Matchers {
  val testInput =
    """London to Dublin = 464
      |London to Belfast = 518
      |Dublin to Belfast = 141""".stripMargin

  val exampleGraph = Graph(
    Set("London", "Dublin", "Belfast").map(Node(_)),
    Map(
      Set(Node("London"), Node("Dublin")) -> 464,
      Set(Node("London"), Node("Belfast")) -> 518,
      //            Node("Dublin") -> (Node("London"), 464),
      Set(Node("Dublin"), Node("Belfast")) -> 141,
      //            Node("Belfast") -> (Node("London"), 518),
      //            Node("Belfast") -> (Node("Dublin"), 141),
    ),
  )

  "Parser" should {

    "parse edge" in {
      Parser.parse(Day9.edge(_))("London to Dublin = 464") should be (
        Some((Node("London"), Node("Dublin"), 464))
      )
    }

    "parse edges from example input" in {
      Parser.parse(Day9.edges(_))(testInput) should be (Some(
        Seq(
          (Node("London"), Node("Dublin"), 464),
          (Node("London"), Node("Belfast"), 518),
          (Node("Dublin"), Node("Belfast"), 141),
        )
      ))
    }

    "parse example input" in {
      Day9.parse(testInput) should be(
        exampleGraph
      )
    }
  }

  "Day 9" should {
    "solve example" in {
      Day9.part1(exampleGraph) should be (605)
    }
  }
}
