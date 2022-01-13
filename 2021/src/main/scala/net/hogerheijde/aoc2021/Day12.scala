package net.hogerheijde.aoc2021

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day12 extends Day[Int, Int] {
  type Model = CaveSystem

  case class Path(reversed: List[Cave]) extends Ordered[Path] {
    def path: List[Cave] = reversed.reverse
    def lastCave: Cave = reversed.head
    def append(cave: Cave): Path = Path(cave +: reversed)
    def visitedSmall(cave: Cave): Boolean = cave match {
      case small: Cave.Small => reversed.contains(small)
      case Cave.Start => true
      case _ => false
    }
    override def toString: String = reversed.reverse.mkString(", ")

    override def compare(that: Path): Int = this.toString.compareTo(that.toString)
  }
  object Path {
    def apply(p: String): Path = Parser.parse(path(_))(p).get
  }
  case class Paths(paths: Set[Path]) {
    def size: Int = paths.size
    override def toString: String = paths.toSeq.sorted.mkString("\n")
  }

  case class CaveSystem(caves: Set[Cave], links: Map[Cave, Set[Cave]]) {
    def toDot: String = {

      val linkedCaves = links
        .flatMap { case (cave1, linkedToCaves) =>
          linkedToCaves.map(cave2 => Set(cave1, cave2))
        }
        .toSeq
        .distinct
        .map(_.toSeq.sorted)
        .sortBy(_.headOption)

      val caves = linkedCaves
        .map(_.mkString(" -- "))
        .mkString("    ", ";\n    ", ";")
      s"""graph caves {
        |$caves
        |}
        |""".stripMargin
    }
  }
  sealed trait Cave extends Ordered[Cave] {
    def name: String
    def compare(o: Cave): Int = this.name.compareTo(o.name)
    override def toString: String = name
  }
  sealed trait MiddleCave extends Cave

  object Cave {
    def apply(value: String): Cave = {
      value match {
        case "start" => Cave.Start
        case "end" => Cave.End
        case x if x.forall(_.isUpper) => Cave.Big(x)
        case x if x.forall(_.isLower) => Cave.Small(x)
        case x => throw new RuntimeException(
          s"I didn't solve this case for all unicode chars, so this doesn't work for input `$x`. " +
            s"Nor does it work for mixed case cave-names."
        )
      }
    }
    case object Start extends Cave { val name = "start"}
    case object End extends Cave { val name = "end" }

    case class Small(name: String) extends MiddleCave { require(name.forall(_.isLower)) }
    case class Big(name: String) extends MiddleCave { require(name.forall(_.isUpper)) }
  }

  def system[_: P]: P[CaveSystem] = P(link.rep).map { links =>
    val allCaves = links.flatMap(l => Set(l._1, l._2)).toSet
    val system = allCaves.foldLeft(Map.empty[Cave, Set[Cave]]) { case (system, nextCave) =>
      val connectedTo = links
        // cave could be left or right part of the link
        .filter { case (from, to) => from == nextCave || to == nextCave }
        // Flatten to single set
        .flatMap(l => Set(l._1, l._2))
        .toSet
        // exclude cave itself.
        .filterNot(_ == nextCave)
      system.updated(nextCave, connectedTo)
    }
    CaveSystem(allCaves, system)
  }
  def link[_: P]: P[(Cave, Cave)] = P(cave ~ "-" ~ cave ~ "\n")
  def cave[_: P]: P[Cave] = P(startCave | endCave | smallCave | bigCave)
  def startCave[_: P]: P[Cave.Start.type] = P("start").map(_ => Cave.Start)
  def endCave[_: P]: P[Cave.End.type] = P("end").map(_ => Cave.End)
  def smallCave[_: P]: P[Cave.Small] = P(CharIn("a-z").rep(min = 1, max = 2).!).map(Cave.Small(_))
  def bigCave[_: P]: P[Cave.Big] = P(CharIn("A-Z").rep(min = 1, max = 2).!).map(Cave.Big(_))
  def path[_: P]: P[Path] = P((cave ~ "," ~ " ".?).rep ~ cave).map { case (init, last) => Path((init :+ last).reverse.toList) }

  override def parse(input: String): Model = Parser.parse(system(_))(input).get

  def expandRoutes(
      system: Model,
      allowedToVisit: (Path, Cave) => Boolean = allowedToVisitRule1,
      currentPath: Path = Path(List(Cave.Start)),
  ): Paths = {
    Paths(system
      // find all caves directly connected to the last cave
      .links(currentPath.lastCave)
      // remove small caves already visited
      .filter(allowedToVisit(currentPath, _))
      .flatMap { nextCave =>
        val newPath = currentPath.append(nextCave)
        nextCave match {
          case Cave.End => Set(newPath)
          case _ => expandRoutes(system, allowedToVisit, newPath).paths
        }
      })
  }

  val allowedToVisitRule1: (Path, Cave) => Boolean = { case (path, cave) =>
    cave match {
      case small: Cave.Small => !path.reversed.contains(small)
      case Cave.Start => false
      case _ => true
    }
  }

  val allowedToVisitRule2: (Path, Cave) => Boolean = {
    case (path, cave) =>
      cave match {
        case small: Cave.Small =>
          val histogram = path
            .reversed
            .collect { case s: Cave.Small => s }
            .groupMapReduce(identity)(_ => 1)(_ + _)
          if (histogram.values.toSet.contains(2)) {
            // Already visited a small cave twice. Cannot visit this small cave more than once
            !histogram.get(small).exists(count => count >= 1)
          } else {
            // Not yet visited a small cave twice. Can visit this small cave more than once.
            !histogram.get(small).exists(count => count >= 2)
          }
        case Cave.Start => false
        case _ => true
      }
  }

  override def part1(input: Model): Int = expandRoutes(
    input,
    allowedToVisitRule1,
  ).size
  override def part2(input: Model): Int = expandRoutes(
    input,
    allowedToVisitRule2,
  ).size
}
