package net.hogerheijde.aoc2015

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day9 extends Day[Int, Int] {
  override type Model = Graph

  override def parse(input: String): Model =
    Parser.parse(graph(_))(input).get


  override def part1(input: Model): Int = {
    val shortest = input.routes.minBy(_._2)
    println(s"Shortest path: ${shortest._1.mkString(", ")} : ${shortest._2}")
    shortest._2
  }
  override def part2(input: Model): Int = {
    val longest = input.routes.maxBy(_._2)
    println(s"Longest path: ${longest._1.mkString(", ")} : ${longest._2}")
    longest._2
  }

  def node[_: P]: P[Node] = P(CharIn("A-Za-z").rep.!).map(Node(_))
  def edges[_: P]: P[Seq[(Node, Node, Int)]] = P(edge.rep)
  def edge[_: P]: P[(Node, Node, Int)] = P(node ~ " to " ~ node ~ " = " ~ int ~ "\n".?)
  def graph[_: P]: P[Graph] = P(edges).map { edges =>
    edges.foldLeft(Graph(Set(), Map())) { case (acc, next) =>
      acc.copy(
        nodes = acc.nodes ++ Set(next._1, next._2),
        edges = acc.edges + (Set(next._1, next._2) -> next._3)
      )
    }
  }
}

case class Node(name: String) {
  override def toString: String = name
}

case class Graph(
    nodes: Set[Node],
    edges: Map[Set[Node], Int],
) {
  lazy val routes = nodes.toList.permutations.map { route =>
    (route, route.sliding(2).map(adjacentNodes => edges(adjacentNodes.toSet)).sum)
  }.toList
}
