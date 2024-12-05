package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.common.parser.Common.intSeq
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

import scala.util.Try

object Day5 extends Day[Int, Int]:

  type Model = PrinterInstructions

  override def parse(input: String): Model = Parser.parse(printerRules(_))(input).get

  override def part1(input: Model): Int =
    input.edits.filter(_.isValid(input.ruleSet)).map(_.middle).sum

  override def part2(input: Model): Int =
    input.edits.filterNot(_.isValid(input.ruleSet)).map(_.sorted(input.ruleSet).middle).sum

  case class PrinterInstructions(ruleSet: RuleSet, edits: Seq[Edits])
  case class Rule(before: Int, after: Int)
  case class RuleSet(rules: Seq[Rule]) extends Ordering[Int]:
    val mustComeAfter: Map[Int, Set[Int]] =
      rules.groupMap( _.before)( _.after).map { case (k, v) => (k, v.toSet) }.withDefaultValue(Set.empty[Int])
    val mustComeBefore: Map[Int, Set[Int]] =
      rules.groupMap( _.after)( _.before).map { case (k, v) => (k, v.toSet)}.withDefaultValue(Set.empty[Int])

    override def compare(x: Int, y: Int): Int =
      if (x == y)
        0
      else
        if (mustComeAfter(x).contains(y))
          -1
        else if (mustComeBefore(x).contains(y))
          1
        else
          throw RuntimeException(s"Cannot determine order of $x and $y")



  case class Edit(page: Int, before: Set[Int], after: Set[Int]):
    def isValid(rules: RuleSet): Boolean =
      rules.mustComeBefore(page).intersect(after).isEmpty &&
        rules.mustComeAfter(page).intersect(before).isEmpty
    override def toString: String = s"${before.toSeq.sorted.mkString(",")}|$page|${after.toSeq.sorted.mkString(",")}"


  case class Edits(edits: IndexedSeq[Int]):
    val middle = edits(edits.length / 2)
    val _edits: Seq[Edit] = edits.zipWithIndex.map { case (edit, index) =>
      Edit(edit, edits.take(index).toSet, edits.drop(index + 1).toSet)
    }
    def isValid(rules: RuleSet): Boolean = _edits.forall(_.isValid(rules))
    def sorted(implicit ord: Ordering[Int]) = Edits(edits.sorted(ord))

  def printerRules[$: P] = P(rules ~ "\n" ~ edits).map(PrinterInstructions(_, _))

  def rule[$: P]: P[Rule] = P(int ~ "|" ~ int ~ "\n").map(Rule(_, _))
  def rules[$: P]: P[RuleSet] = P(rule.rep).map(RuleSet(_))
  def edits[$: P]: P[Seq[Edits]] = P((intSeq ~ "\n").rep).map(_.map(Edits(_)))