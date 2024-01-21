package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.long
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.util.memoize

import java.lang.System.nanoTime
import java.time.Duration
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Day5 extends Day[Long, Long]:

  type Model = Rules
  type Ingredient = String
  type Needle = (Long, Ingredient)

  def parse(input: String): Model = Parser.parse(field(_))(input).get

  def part1(input: Model): Long =
    input.seeds
      .map(seed => resolve(input.mappings, "location")((seed, "seed")))
      .map(_._1)
      .min

  // This will take about 100 minutes to finish 😨.
  def part2(input: Model): Long =
    val space = input.seeds.sliding(2, 2).map { case Seq(_, b) => b }.sum.toInt
    println(s"Looking for $space seeds")
    val start = nanoTime
    input.seeds
      .sliding(2, 2)
      .flatMap { case Seq(start, range) => start until start + range }
      .map(seed => resolve(input.mappings, "location")((seed, "seed")))
      .zipWithIndex
      .foldLeft(Long.MaxValue) { case (min, ((next, _), index)) =>
        if (index % 1000 == 0) {
          val percentage = Math.round((index.toDouble / space) * 10000) / 100.0
          val d = Duration.ofNanos(nanoTime - start)
          print(f"\r$index%10s // $percentage%% // $d // $min%10s // $next%10s")
          System.out.flush()
        }
        Math.min(min, next)
      }

  @tailrec
  def resolve(mappings: Map[Ingredient, Mapping], target: Ingredient)(start: Needle): Needle =
    start._2 match
      case `target` => start
      case _ => resolve(mappings, target)(find(mappings)(start))

  def find(mappings: Map[Ingredient, Mapping])(needle: Needle): Needle =
    val f: Needle => Needle = memoize { case (source, ingredient) =>
      find(mappings)(source, ingredient)
    }
    f(needle)

  private def find(mappings: Map[Ingredient, Mapping])(source: Long, from: Ingredient): Needle =
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
    reverseMappings: Map[Ingredient, Mapping],
  )
  object Rules:
    def apply(seeds: Seq[Long], mappings: Map[Ingredient, Mapping]): Rules =
      Rules(seeds, mappings, mappings.values.map(m => (m.to, m)).toMap)

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
