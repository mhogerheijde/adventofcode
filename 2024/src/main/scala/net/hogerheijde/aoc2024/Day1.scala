package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.util.Try

object Day1 extends Day[Int, Int]:
  type Model = (Seq[Int], Seq[Int])

  override def parse(input: String): Model = Parser.parse(pairs(_))(input).get

  override def part1(input: Model): Int =
    input._1.sorted.zip(input._2.sorted).map((l, r) => Math.abs(l - r)).sum

  override def part2(input: Model): Int =
    input._1.map { l1 => l1 * input._2.count(l2 => l1 == l2 ) }.sum


  def pairs[$: P]: P[Model] = P((pair ~ "\n").rep()).map(_.unzip)
  def pair[$: P]: P[(Int, Int)] = P(int ~ "   " ~ int)
