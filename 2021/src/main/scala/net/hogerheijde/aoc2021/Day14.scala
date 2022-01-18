package net.hogerheijde.aoc2021

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day14 extends Day[Long, Long] {

  type Model = Polymer
  type Pair = (Char, Char)

  case class Polymer(
      firstLast: Pair,
      histogram: Map[Pair, Long],
      rules: Map[Pair, Set[Pair]],
  ) {
    def elementCount: Map[Char, Long] = {
      histogram
        .groupBy { case ((first, _), _) => first }
        .map { case (char, pairCount) => (char, pairCount.values.sum) }
        //        .updatedWith(firstLast._1)(_.map(_ + 1L).orElse(Some(1L)))
        .updatedWith(firstLast._2)(_.map(_ + 1L).orElse(Some(1L)))
    }

    def next(amount: Int): Polymer = {
      Range(0, amount).foldLeft(this)((p, _) => p.next)
    }

    def next: Polymer = {
      val newHistogram = histogram.keys
        // Start with an empty map and add new counts per pair after resolving the rule for that pair.
        .foldLeft(Map.empty[Pair, Long]) { case (hist, nextPair) =>
          // Take current count form the original histogram,
          // since the variable in the fold changes depending on the added pairs
          val currentCount = histogram(nextPair)

          rules.get(nextPair) match {
            case Some(pairs) =>
              // Add
              pairs.foldLeft(hist) { (additions, p) =>
                additions.updatedWith(p)(_.map(_ + currentCount).orElse(Some(currentCount)))
              }
            case None =>
              // I don't think the puzzle ever produces rules that end up in a
              // pair that doesn't have a rule.
              // For completeness, however, if a pair exists that doesn't have a rule:
              // keep the current count.
              hist.updated(nextPair, currentCount)
          }
        }


      this.copy(histogram =
        newHistogram
      )
    }
  }

  def element[_: P]: P[Char] = P(CharIn("A-Z").!).map(_.toCharArray.head)

  def polymer[_: P]: P[Polymer] = P(template ~ "\n" ~ rules).map { case (template, firstLast, rules) =>
    Polymer(firstLast, template, rules)
  }

  def template[_: P]: P[(Map[Pair, Long], Pair)] = P(element.rep ~ "\n").map { template =>
    val histogram = template.toSeq.sliding(2).map {
      case Seq(first, second) => (first, second)
      case _ => throw new RuntimeException("Sliding 2 should guarantee 2 items")
    }
      .toSeq
      .groupBy(identity)
      .view.mapValues { q => q.size.toLong }
      .toMap
    (histogram, (template.head, template.last))
  }

  def rules[_: P]: P[Map[Pair, Set[Pair]]] = P((rule ~ "\n").rep).map(_.toMap)

  def rule[_: P]: P[(Pair, Set[Pair])] = P(element ~ element ~ " -> " ~ element).map { case (first, second, insert) =>
    (
      first -> second, // from this pair
      Set(
        first -> insert, // to these pairs (i.e. insert the insert in between
        insert -> second, // for `AB -> C' this would give `ACB` or in our system pairs ('A', 'C") and ('C', 'B').
      )
    )
  }


  override def parse(input: String): Model = Parser.parse(polymer(_))(input).get

  override def part1(input: Model): Long = {
    val elements = input.next(10).elementCount
    val max = elements.maxBy(_._2)._2
    val min = elements.minBy(_._2)._2

    max - min
  }

  override def part2(input: Model): Long = {
    val elements = input.next(40).elementCount
    val max = elements.maxBy(_._2)._2
    val min = elements.minBy(_._2)._2

    max - min
  }
}
