package net.hogerheijde.aoc2017.day7

import scala.collection.immutable.IndexedSeq

case class Disc(name: String, weight: Int)

object Disc {
  type TowersByDisc = Map[Disc, IndexedSeq[Disc]]
  type DiscsByName = Map[String, Disc]

  def fromString(string: String): Disc = {
    val parts = string.trim.split(" ")
    val weight = Integer.parseInt(parts(1).drop(1).dropRight(1))
    Disc(name = parts(0), weight = weight)
  }
}
