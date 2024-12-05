package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day3.Instruction.Garbage
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Do
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Dont
import net.hogerheijde.aoc2024.Day3.Instruction.Usefull.Multiply

import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.util.Try
import scala.util.matching.Regex

object Day3 extends Day[Int, Int]:

  type Model = Seq[Instruction]

  override def parse(input: String): Model = Parser.parse(program(_))(input).get

  override def part1(input: Model): Int = input.collect { case m: Multiply => m }.map(_.evaluate()).sum
  override def part2(input: Model): Int = input.foldLeft((0, false)) { case ((total, muted), next) =>
    next match {
      case g: Garbage => (total, muted)
      case m: Multiply => if (muted) (total, muted) else (total + m.evaluate(), muted)
      case Do => (total, false)
      case Dont => (total, true)
    }
  }._1

  sealed trait Instruction
  object Instruction:
    case class Garbage(s: String) extends Instruction:
      def toOption: Option[Garbage] = if (s == "") None else Some(this)
    sealed trait Usefull extends Instruction
    object Usefull:
      object Dont extends Usefull
      object Do extends Usefull
      case class Multiply(x: Int, y: Int) extends Usefull:
        def evaluate(): Int = x * y
      object Multiply:
        def apply(s: String): Multiply = Parser.parse(multiply(_))(s).get


  // There's probably a neater way to do this, but I couldn't get fastparse to stop using all memory
  def program[$: P]: P[Seq[Instruction]] = P(garbage.? ~ (useful ~ garbage.?).rep).map {
    case (g1, is) => (g1 +: is.flatMap { case (x, y) => Seq(Some(x), y.flatMap(_.toOption)) }).flatten
  }
  def useful[$: P]: P[Usefull] = P(multiply | `do` | dont)
  def multiply[$: P]: P[Multiply] = P("mul(" ~ int ~ "," ~ int ~ ")").map((x, y) => Multiply(x, y))
  def dont[$: P]: P[Dont.type] = P("don't()").map(_ => Dont)
  def `do`[$: P]: P[Do.type] = P("do()").map(_ => Do)
  def garbage[$: P]: P[Garbage] = P((!useful ~ AnyChar.! ~/ "").rep).map(x => Garbage(x.mkString("")))

