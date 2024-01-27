package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.CircularBuffer
import net.hogerheijde.aoc.util.CircularBuffer.NonEmptyCircularBuffer
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day8.Direction.L
import net.hogerheijde.aoc2023.Day8.Direction.R

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object Day8 extends Day[Int, Long]:

  type Model = (Directions, Graph)

  override def parse(input: String): Model = Parser.parse(game(_))(input).get

  override def part1(input: Model): Int = steps(input._1, input._2, "AAA", _ == "ZZZ", 0)

  override def part2(input: Model): Long =
    val countPerStartNode = startNodes(input._2).map(n =>
      steps(input._1, input._2, n, (n) => n.endsWith("Z"), 0)
    )
    countPerStartNode.toSeq.lcm


  @tailrec
  def steps(directions: Directions, graph: Graph, from: Node, endCondition: (Node) => Boolean, count: Int): Int =
    follow(graph, from, directions.headOption.get) match
      case end if endCondition(end) => count + 1
      case node => steps(directions.rotate, graph, node, endCondition, count + 1)

  def startNodes(graph: Graph): Set[Node] = graph.filter(_.endsWith("A"))

  def follow(graph: Graph, from: Node, direction: Direction): Node =
    direction match
      case L => graph(from)._1
      case R => graph(from)._2


  def game[$: P]: P[(Directions, Graph)] = P(directions ~ "\n" ~ graph)

  def direction[$: P]: P[Direction] = P(CharIn("RL").!).map(d => Direction.apply(d))
  def directions[$: P]: P[Directions] = P(direction.rep ~ "\n").map(ds => CircularBuffer(ds:_*))
  def node[$: P]: P[Node] = P(CharIn("A-Z0-9").rep(exactly = 3).!)
  def nodeLine[$: P]: P[(Node, Node, Node)] = P(node ~ " = (" ~ node ~ ", " ~ node ~ ")")
  def graph[$: P]: P[Graph] = P((nodeLine ~ "\n".?).rep).map(nodeLines =>
    Graph(nodeLines.map((from, left, right) => (from, (left, right))).toMap)
  )

  type Node = String
  type Directions = CircularBuffer[Direction]
  enum Direction:
    case R, L

  object Direction:
    def apply(s: String): Direction = s match
      case "L" => L
      case "R" => R
      case _ => throw new IllegalStateException(s"Cannot create direction for $s")

  case class Graph(nodes: Map[Node, (Node, Node)]):
    def apply(node: Node): (Node, Node) = nodes(node)
    def filter(f: (Node) => Boolean): Set[Node] = nodes.keySet.filter(f)
    def asDot: String =
      val foo: Seq[String] = nodes.map((from, to) => s"$from -> {${to._1} ${to._2}}").toSeq
      s"""strict digraph {
         |  AAA[color=green];
         |  ZZZ[color=red];
         |${foo.sorted.mkString("  ",  ";\n  ", ";")}
         |}""".stripMargin


  extension (s: Seq[Int])
    def lcm: Long =
      @tailrec
      def gcd(a: Long, b: Long): Long =
        if (b == 0) a.abs else gcd(b, a % b)

      s.foldLeft(1L)((a, b) => (a / gcd(a, b)) * b)
