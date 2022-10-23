package net.hogerheijde.aoc.graph


case class Node(label: String)
case class WeightedGraph(nodes: Map[Node, (Node, Long)])


