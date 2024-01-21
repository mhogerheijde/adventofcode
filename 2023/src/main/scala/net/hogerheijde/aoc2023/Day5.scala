package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.long
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.annotation.tailrec
import scala.util.Try

object Day5 extends Day[Long, Long]:

  type Model = Rules // (Seq[Long], Map[Ingredient, Mapping])
  type Ingredient = String

  override def parse(input: String): Model = Parser.parse(field(_))(input).get

  override def part1(input: Model): Long =
    input.seeds
      .map(seed => resolve(input.mappings, "location")((seed, "seed")))
      .map(_._1)
      .min

  override def part2(input: Model): Long =
    input.seeds.sliding(2, 2)
      .flatMap(pair => pair.head until pair.head + pair(1))
      .map(seed => resolve(input.mappings, "location")((seed, "seed")))
      .map(_._1)
      .min




  @tailrec
  def resolve(mappings: Map[Ingredient, Mapping], target: Ingredient)(start: (Long, Ingredient)): (Long, Ingredient) =
    start._2 match
      case `target` => start
      case _ => resolve(mappings, target)(find(mappings)(start))

  def find(mappings: Map[Ingredient, Mapping])(needle: (Long, Ingredient)): (Long, Ingredient) =
    find(mappings)(needle._1, needle._2)
  def find(mappings: Map[Ingredient, Mapping])(source: Long, from: Ingredient): (Long, Ingredient) =
    mappings.get(from) match
      case None => throw IllegalArgumentException(s"Can't find mapping for ingredient $from")
      case Some(mapping) => (mapping.map(source), mapping.to)

  private def field[$: P]: P[Model] = P(seeds ~ mapping.rep).map((seeds, mappings) =>
    Rules(seeds, mappings.map(m => (m.from, m)).toMap)
  )
  private def seeds[$: P]: P[Seq[Long]] = P("seeds:" ~ (" " ~ long).rep(min=1) ~ "\n")
  private def range[$: P]: P[Range] = P(long ~ " " ~ long ~ " " ~ long ~ "\n")
    .map((dest, source, range) => Range(source, dest, range))

  private def name[$: P]: P[String] = P(CharIn("a-z").rep(min=1).!)
  private def mapping[$: P]: P[Mapping] = P("\n" ~ name ~ "-to-" ~ name ~ " map:\n" ~ range.rep)
    .map((from, to, ranges) => Mapping(from, to, ranges))

  case class Rules(
    seeds: Seq[Long],
    mappings: Map[Ingredient, Mapping],
  )

  case class Range(source: Long, dest: Long, size: Long):
    private val offset = dest - source
    private val endExclusive = source + size
    def map(input: Long): Option[Long] =
      if (input >= source && input < endExclusive)
        Some(input + offset)
      else
        None

  case class Mapping(
    from: Ingredient,
    to: Ingredient,
    ranges: Seq[Range],
  ):
    def map(input: Long): Long = ranges.map(_.map(input)).find(i => i.isDefined).getOrElse(Some(input)).get

//  object Mapping:
//    def apply(from: String, to: String, ranges: Seq[Range]): Mapping = Mapping(
//      from: Ingredient,
//      to: Ingredient,
//      ranges,
//    )
//
//  object Ingredient:
//    def apply(i: String): Ingredient = i
