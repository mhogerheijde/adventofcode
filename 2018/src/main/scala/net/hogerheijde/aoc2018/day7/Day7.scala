package net.hogerheijde.aoc2018.day7

import fastparse.NoWhitespace._
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018

case class Graph(arcs: IndexedSeq[Arc]) {

  val root: IndexedSeq[Char] = {
    val froms = arcs.map(_.from).toSet
    val tos = arcs.map(_.to).toSet
    (froms -- tos).toIndexedSeq.sorted
  }

  def toDot: String = {
    "digraph TheGRaph {\n" +
        arcs.map(g => s"${g.from} -> ${g.to}").mkString("\n") +
      "\n}"
  }

}

case class Arc(from: Char, to: Char)

object Day7 extends Day2018[Graph, String, Unit]{


  def arc[_: P]: P[Arc] = P("Step " ~ SingleChar.! ~ " must be finished before step " ~ SingleChar.! ~" can begin.").map {
    case (from, to) => Arc(from.head, to.head)
  }
  def graph[_: P]: P[Graph] = P((arc ~ "\n".?).rep).map(arcs => Graph(arcs.toIndexedSeq))


  override def name: String = "Day 7"

  override def parse(input: String): Graph = {
    Parser.parse(graph(_))(input).get // allow to throw
  }

  override def part1(input: Graph): String = {
    println(input.toDot)

    input.root.toString
  }

  override def part2(input: Graph): Unit = ???
}
