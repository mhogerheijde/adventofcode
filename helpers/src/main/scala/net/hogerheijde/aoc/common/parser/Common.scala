package net.hogerheijde.aoc.common.parser

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate

import scala.collection.immutable.IndexedSeq

object Common {

  def intSeq[_: P]: P[IndexedSeq[Int]] = P((int ~ " ".rep.?).rep).map(_.toIndexedSeq)

  def int[_: P]: P[Int] = P(("-".? ~ CharIn("0-9").rep(1)).!.map(_.toInt))
  def coordinate[_: P]: P[Coordinate] = P((int ~ "," ~ " ".rep ~ int).map(t => Coordinate(t._1, t._2)))
}
