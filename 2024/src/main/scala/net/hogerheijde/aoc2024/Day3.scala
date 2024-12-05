package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.MultiLineWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.util.Try
import scala.util.matching.Regex

object Day3 extends Day[Int, Int]:

  type Model = String

  override def parse(input: String): Model = input
//    regex
//    .findAllMatchIn(input)
//    .flatMap { q =>
//      println(q.subgroups)
//      q.subgroups match {
//        case List("", m) => Seq(Multiply(m))
//        case List(g, m) => Seq(Garbage(g), Multiply(m))
//        case _ => Seq[Instruction]()
//      }
//    }
//    .toSeq

  def findMultiply(input: Model): Seq[Instruction] =
    raw"mul\([0-9]+,[0-9]+\)".r
      .findAllMatchIn(input)
      .map { m => Multiply(m.matched) }
      .toSeq


  override def part1(input: Model): Int = findMultiply(input).map(_.evaluate()).sum

  override def part2(input: Model): Int =
    val is: ByteArrayInputStream = ByteArrayInputStream(input.map(_.toByte).toArray)
    is.mark(1)
    var in = is.read()
    while (in != -1)
      in = is.read()

    ???

  sealed trait Instruction:
    def evaluate(): Int = 0

  object Dont extends Instruction
  object Do extends Instruction
  case class Multiply(x: Int, y: Int) extends Instruction:
    override def evaluate() = x * y
  object Multiply:
    def apply(s: String): Multiply = Parser.parse(multiply(_))(s).get
  case class Garbage(s: String) extends Instruction


  val regex = raw"(.*?)(mul\([0-9]+,[0-9]+\))".r

//  def program[$: P]: P[Model] = P(instruction.rep)
//  def instruction[$: P]: P[Instruction] = P(multiply | garbage)
  def multiply[$: P]: P[Multiply] = P("mul(" ~ int ~ "," ~ int ~ ")" ~/ "").map((x, y) => Multiply(x, y))
  def dont[$: P]: P[Dont.type] = P("dont()").map(_ => Dont)
  def `do`[$: P]: P[Do.type] = P("do()").map(_ => Do)
//  def garbage[$: P]: P[Garbage] = P(AnyChar.rep.!).map(Garbage(_))

