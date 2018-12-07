package net.hogerheijde.aoc2018.day7

import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

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
    "find its sources" in {
      exampleGraph.source should be (IndexedSeq('C'))
    }

    "find its sinks" in {
      exampleGraph.sink should be (IndexedSeq('E'))
    }
    "find children" in {
      exampleGraph.childrenOf('C') should be (IndexedSeq('A', 'F'))
      exampleGraph.childrenOf('A') should be (IndexedSeq('B', 'D'))
      exampleGraph.childrenOf('F') should be (IndexedSeq('E'))
    }

    "find next available things" in {
      exampleGraph.work(IndexedSeq(), IndexedSeq('C')) should be(
        (IndexedSeq('C'), IndexedSeq('A', 'F')))

      exampleGraph.work(IndexedSeq('C'), IndexedSeq('A', 'F')) should be(
        (IndexedSeq('C', 'A'), IndexedSeq('B', 'D', 'F')))

      exampleGraph.work(IndexedSeq('C', 'A'), IndexedSeq('B', 'D', 'F')) should be(
        IndexedSeq('C', 'A', 'B'), IndexedSeq('D', 'F'))

      exampleGraph.work(IndexedSeq('C', 'A', 'B'), IndexedSeq('D', 'F', 'E')) should be(
        IndexedSeq('C', 'A', 'B', 'D'), IndexedSeq('F'))

      exampleGraph.work(IndexedSeq('C', 'A', 'B', 'D'), IndexedSeq('F')) should be(
        IndexedSeq('C', 'A', 'B', 'D', 'F'), IndexedSeq('E'))

      exampleGraph.work(IndexedSeq('C', 'A', 'B', 'D', 'F'), IndexedSeq('E')) should be(
        IndexedSeq('C', 'A', 'B', 'D', 'F', 'E'), IndexedSeq())
    }

    "find sequence" in {
      exampleGraph.sequence should be ("CABDFE")
    }

  }
}
