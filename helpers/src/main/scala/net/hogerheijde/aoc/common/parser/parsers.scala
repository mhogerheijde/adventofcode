package net.hogerheijde.aoc.common.parser

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.common.model

object Common:
  def digit[$: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt)
  def alphaLower[$: P]: P[String] = P(CharIn("a-z").rep(min = 1).!)
  def intSeq[$: P]: P[IndexedSeq[Int]] = P((int ~ ("," | ";").? ~ " ".rep.?).rep).map(_.toIndexedSeq)

  def int[$: P]: P[Int] = P("-".? ~ uint).map(_.toInt)
  def uint[$: P]: P[Int] = P(CharIn("0-9").rep(min = 1).!).map(_.toInt)

  def long[$: P]: P[Long] = P("-".? ~ ulong).map(_.toLong)
  def ulong[$: P]: P[Long] = P(CharIn("0-9").rep(min = 1).!).map(_.toLong)

  def coordinate[$: P]: P[Coordinate] = P((int ~ "," ~ " ".rep ~ int).map(t => Coordinate(t._1, t._2)))
  def coordinates[$: P]: P[Seq[Coordinate]] = P((coordinate ~ "\n").rep)

