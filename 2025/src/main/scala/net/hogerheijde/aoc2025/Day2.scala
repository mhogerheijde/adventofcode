package net.hogerheijde.aoc2025

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.ulong
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day2 extends Day[Long, Long]:

  type Model = Seq[Range]

  case class Range(start: Long, end: Long):
    def filter(f: Long => Boolean): Seq[Long] =
      (start to end).filter(f)

    def filterNot(f: Long => Boolean): Seq[Long] =
      (start to end).filterNot(f)

  object Range:
    def apply(start: Int, end: Int): Day2.Range = Day2.Range(start.toLong, end.toLong)


  override def parse(input: String): Model = Parser.parse(ranges(_))(input).get

  override def part1(input: Model): Long = input.flatMap { range =>
    range.filterNot(isValidPart1)
  }.sum

  override def part2(input: Model): Long = input.flatMap { range =>
    range.filterNot(isValidPart2)
  }.sum

  def isValidPart1(i: Long): Boolean =
    val s = i.toString
    s.take(s.length / 2) != s.drop(s.length / 2)

  def isValidPart2(i: Long): Boolean =
    val s = i.toString
    !(1 to (s.length / 2)).exists { n =>
      val pattern = s.take(n)
      s.drop(n).sliding(n, n).forall(_ == pattern)
    }

  def range[$: P]: P[Range] = P (ulong ~ "-" ~ ulong).map { case (start, end) => Day2.Range(start, end) }
  def ranges[$: P]: P[Seq[Range]] = ((range ~ "," ).rep() ~ range ~ "\n").map { case (seq, last) => seq :+ last }