package net.hogerheijde.aoc2017.day7

import net.hogerheijde.aoc2017.Day

import scala.collection.immutable.IndexedSeq

object Day7 extends Day[Map[Disc, IndexedSeq[Disc]], String, Unit] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 7"
  override def parse: String => Map[Disc, IndexedSeq[Disc]] = { input =>
    val pass1 = input.lines.map { line =>
      val parts = line.split(" -> ")
      val disc: Disc = Disc.fromString(parts(0))

      val carrying = if (parts.length > 1) {
        parts(1).split(", ").toIndexedSeq.map(_.trim)
      } else { IndexedSeq() }

      (disc, carrying)
    }.toIndexedSeq

    val disksByName = pass1.map { case (disc, _) => (disc.name, disc) }.toMap

    pass1.map { case (disc, carrying) =>
      val discsMapped = carrying.map { name =>
        disksByName(name)
      }
      (disc, discsMapped)
    }.toMap
  }
  override def part1(input: Map[Disc, IndexedSeq[Disc]]): String = {

    val outgoingRelationships = input.foldLeft(Set.empty[(String, String)]) { case (relations, (disc, carrying)) =>
        val currentRelations = carrying.map( carried => (disc.name, carried.name) ).toSet
        relations ++ currentRelations
    }

    val (carryingDiscs, discsBeingCarried) = outgoingRelationships.unzip

    val rootDiscs = carryingDiscs -- discsBeingCarried

    if (rootDiscs.size == 1) {
      rootDiscs.head
    } else {
      throw new RuntimeException(s"Something went wrong; got these rootdisks: $rootDiscs")
    }


  }
  override def part2(input: Map[Disc, IndexedSeq[Disc]]): Unit = ???
}

case class Disc(name: String, weight: Int)

object Disc {
  def fromString(string: String): Disc = {
    val parts = string.trim.split(" ")
    val weight = Integer.parseInt(parts(1).drop(1).dropRight(1))
    Disc(name = parts(0), weight = weight)
  }
}
