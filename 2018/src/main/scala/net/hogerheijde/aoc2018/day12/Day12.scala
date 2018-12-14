package net.hogerheijde.aoc2018.day12

import scala.collection.immutable.IndexedSeq

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.util.Implicits._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day12.State.withPadding

object Day12 extends Day2018[Farm, Int, String]{
  override def name: String = "Day 12"

  override def parse(input: String): Farm = Farm.parse(input)

  override def part1(input: Farm): Int = ???

  override def part2(input: Farm): String = ???
}

case class Farm(state: State, rules: Map[Pattern, Space]) {


  def next: Farm = {
    val newState = state.slideWith(rules.withDefaultValue(Pot))
    this.copy(state = newState)
  }

  override def toString: String = s"$state\n${rules.values.mkString("\n")}"
}

object Farm {
  private def space[_: P]: P[Space] = P("#".! | ".".!).map { _ match {
    case "#" => Plant
    case "." => Pot
  }}

  private def rule[_: P]: P[Rule] = P(space.rep(exactly = 5) ~ " => " ~ space ~ "\n".?).map { case (spaces, result) =>
    val pattern = Pattern(spaces(0), spaces(1), spaces(2), spaces(3), spaces(4)) // fastparse guarantees exaclty 5 spots
    Rule(pattern, result)
  }

  private def initialState[_: P]: P[State] = P("initial state: "~ space.rep ~ "\n").map { x =>
    val spaces = x.toIndexedSeq
    State(spaces)
  }
  private def farm[_: P]: P[Farm] = P(initialState ~ "\n" ~ rule.rep).map { case (state, rules) =>
    val ruleMapping = rules.groupBy(_.pattern).mapValues(_.head.result)
    Farm(state, ruleMapping)
  }

  def parse(input: String): Farm =
    Parser.parse(farm(_))(input) match {
      case Some(farm) => farm
      case _ => throw new RuntimeException("Couldn't parse the thing")
  }

  def apply(state: State, rules: Iterable[Rule]): Farm = {
    Farm(state, rules.groupByUnique(_.pattern).mapValues(_.result))
  }
}

sealed trait Space
case object Plant extends Space {
  override val toString: String = "#"
}
case object Pot extends Space {
  override val toString: String = "."
}


case class Pattern(_1: Space, _2: Space, _3: Space, _4: Space, _5: Space) {
  override def toString: String = s"${_1}${_2}${_3}${_4}${_5}"
}
object Pattern {
  def fromList(spaces: List[(Int, Space)]): Pattern = {
    require(spaces.size == 5, "Length of pattern must be 5")
    Pattern(spaces(0)._2, spaces(1)._2, spaces(2)._2, spaces(3)._2, spaces(4)._2)
  }
}

case class Rule(pattern: Pattern, result: Space) {
  override def toString: String = s"$pattern => $result"
}

class State private(spaces: List[(Int, Space)]) {

  def slideWith(f: Pattern => Space): State = {
    val result = spaces.sliding(5).foldLeft (List.empty[(Int, Space)]) { case (newSpaces, pattern) =>
      val spaceNo = pattern(2)._1
      val p = Pattern.fromList(pattern)
      val newSpace = (spaceNo, f(p))
      newSpace +: newSpaces
    }
    new State(withPadding(result.reverse))
  }

  override def toString: String = {
    spaces.map(_._2).mkString("")
  }
}

object State {

  private def withPadding(spaces: List[(Int, Space)]): List[(Int, Space)] = {
    val amountToPrepend = 5 - spaces.take(5).count(_._2 == Pot)
    val amountToPostpend = 5 - spaces.reverse.take(5).count(_._2 == Pot)

    val min = spaces.head._1
    val max = spaces.last._1
    val prefix = Range(min - amountToPrepend, min) map { i => (i, Pot) }
    val postfix = Range.inclusive(max + 1, max + amountToPostpend) map { i => (i, Pot) }

    prefix.toList ++ spaces ++ postfix
  }


  def apply(spaces: IndexedSeq[Space]): State = {
    val indexed = spaces.zipWithIndex.map(_.swap).toList
    new State(withPadding(indexed))
  }
}


