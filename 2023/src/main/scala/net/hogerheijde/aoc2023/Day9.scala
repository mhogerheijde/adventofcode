package net.hogerheijde.aoc2023

import net.hogerheijde.aoc.util.Day

import scala.annotation.tailrec
import scala.util.Try

object Day9 extends Day[Long, Long]:

  type Model = Seq[Seq[Long]]

  override def parse(input: String): Model = input.linesIterator.map(_.split(" ").toSeq.map(_.toLong)).toSeq

  override def part1(input: Model): Long =
    input.map(s => predict(layers(s))).sum

  override def part2(input: Model): Long =
    input.map(s => predict2(layers(s))).sum

  def nextLayer(s: Seq[Long]): Seq[Long] = s.sliding(2).map { case Seq(a, b) => b - a }.toSeq

  @tailrec
  def layers(s: Seq[Long], currentLayers: Seq[Seq[Long]] = Seq()): Seq[Seq[Long]] =
    nextLayer(s) match
      case r if r.forall(_ == 0) => s +: currentLayers
      case r => layers(r, s +: currentLayers)


  def predict(s: Seq[Seq[Long]]): Long =
    val l = s.map(_.last)
    l.foldLeft(0L)((a, b) => a + b)

  def predict2(s: Seq[Seq[Long]]): Long =
    val h = s.map(_.head)
    h.foldLeft(0L)((a, b) =>
      b - a
    )