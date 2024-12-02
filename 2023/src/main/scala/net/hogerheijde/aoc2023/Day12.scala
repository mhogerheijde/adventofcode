package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day10.Tile
import net.hogerheijde.aoc2023.Day12.Group.Broken
import net.hogerheijde.aoc2023.Day12.Group.Unknown
import net.hogerheijde.aoc2023.Day12.Group.Working

import scala.annotation.targetName
import scala.util.Try

object Day12 extends Day[Int, Int]:

  type Checksum = Seq[Int]
  type Model = Seq[(Pattern, Checksum)]

  override def parse(input: String): Model = Parser.parse(rows)(input).get

  override def part1(input: Model): Int = 0

  override def part2(input: Model): Int = 0

  def optionsFor(tail: Pattern, checksums: Seq[Int]): Seq[Pattern] = optionsFor(Pattern(), tail, checksums)
  def optionsFor(init: Pattern, tail: Pattern, checksums: Seq[Int]): Seq[Pattern] =
    checksums match
      case Seq() =>
        if (tail.hasBroken) {
          // Invalid, we have a group of unknown or
          Seq()
        } else if (tail.hasUnknown) {
          Seq(init + tail.map(_.asWorking))
        } else {
          Seq(init + tail)
        }
      case Seq(checksum) =>
        if (tail.hasUnknown) {
          
        } else {
          tail.broken match
            case Seq(broken) if broken.size == checksum => Seq(tail)
            case _ => Seq()
        }
      case _ => Seq()


  case class Pattern(groups: Seq[Group]):
    val broken = groups.collect { case b: Broken => b }
    val hasBroken = broken.nonEmpty

    val unknown = groups.collect { case u: Unknown => u }
    val hasUnknown = unknown.nonEmpty

    val working = groups.collect { case u: Working => u }
    val hasWorking = working.nonEmpty

    def map[T](f: Group => Group): Pattern = copy(groups = groups.map(f(_)))

    @targetName("combineWith")
    def +(other: Pattern): Pattern = Pattern(groups ++ other.groups).canonical

    override def toString: String = groups.mkString("")
    def canonical: Pattern =
      groups match
        case Seq() => this
        case Seq(_) => this
        case _ =>
          val (init, tail) = groups.tail.foldLeft((Seq.empty[Group], groups.head)) { case ((newGroups, last), next) =>
            if (last.getClass == next.getClass) {
              (newGroups, last.merge(next))
            } else {
              (newGroups :+ last, next)
            }
          }
          Pattern(init :+ tail)
  object Pattern:
    def apply(): Pattern = Pattern(Seq())

  sealed trait Group:
    def symbol: String
    def size: Int
    def merge(other: Group): Group
    def asWorking: Group
    override def toString: String = symbol.repeat(size)

  object Group:
    case class Working(size: Int) extends Group:
      override val symbol = "."
      override val asWorking: Working = this
      override def merge(other: Group): Working = Working(size + other.size)

    case class Broken(size: Int) extends Group:
      override val symbol = "#"
      override val asWorking: Broken = this
      override def merge(other: Group): Broken = Broken(size + other.size)

    case class Unknown(size: Int) extends Group:
      override val symbol = "?"
      override val asWorking: Working = Working(size)
      override def merge(other: Group): Unknown = Unknown(size + other.size)

  def working[$: P]: P[Working] = P(".".rep(1).!).map { w => Working(w.length) }
  def broken[$: P]: P[Broken] = P("#".rep(1).!).map { b => Broken(b.length) }
  def unknown[$: P]: P[Unknown] = P("?".rep(1).!).map { u => Unknown(u.length) }
  def group[$: P]: P[Group] = P(working | broken | unknown)
  def pattern[$: P]: P[Pattern] = P(group.rep(1)).map(g => Pattern(g))

  def row[$: P]: P[(Pattern, Checksum)] = P(pattern ~ " " ~ intSeq)
  def rows[$: P]: P[Seq[(Pattern, Checksum)]] = P((row ~ "\n".?).rep)

  extension (i: Int)
    def u: Unknown = Unknown(i)
    def w: Working = Working(i)
    def b: Broken = Broken(i)

  extension (s: String)
    def toPattern: Pattern = Parser.parse(pattern)(s).get
