package net.hogerheijde.aoc2020

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.collection.immutable.Seq

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.common.parser.Common._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser


object Day7 extends Day[Int, Int] {
  type Label = String
  type CountedBag = (Int, Label)
  type Model = Map[Label, Map[Label, Int]]

  override def parse(input: String): Day7.Model = Parser.parse(rules(_))(input).get

  override def part1(input: Day7.Model): Int = input.keySet.count { label => canContain("shiny gold", label, input) }
  override def part2(input: Day7.Model): Int = countContainingBags("shiny gold", input)

  def canContain(needle: Label, bagToCheck: Label, rules: Model): Boolean = {
    rules.get(bagToCheck).exists { rule =>
      rule.keySet.toSeq match {
        case Seq() => false
        case labels => labels.contains(needle) || labels.exists(l => canContain(needle, l, rules))
      }
    }
  }

  def countContainingBags(needle: Label, rules: Model): Int = {
    rules(needle).toSeq match {
      case Seq() => 0
      case bags => bags
        .map { case (label, count) => count + count * countContainingBags(label, rules) }
        .sum
    }
  }


  // ---------- helpers

  def toDot(rules: Model): String = {
    val b = new StringBuilder
    b.append("digraph rules {\n")
    rules.foreach { case (fromLabel, contents) =>
      val from = fromLabel.replace(" ", "")
      b.append(s"""  $from [label="$fromLabel"]\n""")
      contents.foreach { case (toLabel, amount) =>
        val to = toLabel.replace(" ", "")
        b.append(s"""  $to [label="${fromLabel}"]\n""")
        b.append(s"  $from -> $to [label=$amount]\n")
      }
    }
    b.append("}")

    b.toString()
  }


  // ---------- parsers

  def rules[_: P]: P[Map[Label, Map[Label, Int]]] = P(Start ~ (rule ~ "\n".?).rep).map(_.toMap)
  def rule[_: P]: P[(Label, Map[Label, Int])] = P(bag ~ " contain " ~ contents ~ ".")
    .map { case (label, contents) => (label, contents.map(_.swap).toMap) }

  def contents[_: P]: P[Seq[CountedBag]] = P((countedBags ~ ", ".?).rep(1) | nothing)

  def countedBags[_: P]: P[CountedBag] = P(number ~ " "  ~ bag)
  def bag[_: P]: P[Label] = P(label ~ " bag" ~ "s".?)
  def nothing[_: P]: P[Seq[CountedBag]] = P("no other bags".!).map(_ => Seq.empty)

  def label[_: P]: P[Label] = P((alphaLower ~ " " ~ alphaLower).!)
}
