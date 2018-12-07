package net.hogerheijde.aoc2018.day7

import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day7Test extends WordSpec with Matchers {


  val exampleGraph = Day7.parse(
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".stripMargin)


  "Day 7" should {
    "parse example" in {
      exampleGraph should be (
        Graph(IndexedSeq(
          Arc('C', 'A'),
          Arc('C', 'F'),
          Arc('A', 'B'),
          Arc('A', 'D'),
          Arc('B', 'E'),
          Arc('D', 'E'),
          Arc('F', 'E'),
        ))
      )
    }
  }


  "Graph" should {
    "find its root" in {
      exampleGraph.root should be (IndexedSeq('C'))
    }
  }
}
