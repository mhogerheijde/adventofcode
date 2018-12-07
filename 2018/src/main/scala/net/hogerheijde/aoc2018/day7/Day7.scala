package net.hogerheijde.aoc2018.day7

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce

import fastparse.NoWhitespace._
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import scala.collection.immutable.IndexedSeq

case class Graph(arcs: IndexedSeq[Arc]) {

  val nodes = arcs.flatMap(a => Seq(a.from, a.to)).sorted
  val outgoingArcs = arcs.groupBy(_.from).withDefaultValue(IndexedSeq())
  val incomingArcs = arcs.groupBy(_.to).withDefaultValue(IndexedSeq())

  val (source, sink) = {
    val froms = arcs.map(_.from).distinct
    val tos = arcs.map(_.to).distinct
    val r = (froms diff tos).sorted
    val s = (tos diff froms).sorted
    (r, s)
  }

  def toDot: String = {
    "digraph TheGRaph {\n" +
        arcs.map(g => s"${g.from} -> ${g.to}").mkString("\n") +
      "\n}"
  }



  def childrenOf(parent: Char): IndexedSeq[Char] = outgoingArcs(parent).map(_.to)

  def hasAllPrerequisites(ready: IndexedSeq[Char])(part: Char): Boolean = {
    incomingArcs(part).forall(a => ready.contains(a.from))
  }

  def work(ready: IndexedSeq[Char], available: IndexedSeq[Char]): (IndexedSeq[Char], IndexedSeq[Char]) = {

    val newReady = ready :+ available.head
    val newAvailable = (available.tail ++ newReady.flatMap(childrenOf).filterNot(newReady.contains)).sorted.distinct
    val nonBlockedChildren = newAvailable.filter(hasAllPrerequisites(newReady))

    (newReady,  nonBlockedChildren)
  }


  def sequence: String = {
    @tailrec
    def solve(ready: IndexedSeq[Char], available: IndexedSeq[Char]): String = {
      work(ready, available) match {
        case (newReady, IndexedSeq()) =>
          newReady.mkString("")
        case (newReady, newAvailable) =>
          solve(newReady, newAvailable)
      }
    }
    solve(IndexedSeq(), source)
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
    input.sequence
  }

  override def part2(input: Graph): Unit = ???
}
