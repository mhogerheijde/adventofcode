package net.hogerheijde.aoc2018.day12

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.util.Implicits._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day12.Farm.nextState
import net.hogerheijde.aoc2018.day12.State.withPadding

object Day12 extends Day2018[Farm, Long, Long]{
  override def name: String = "Day 12"

  override def parse(input: String): Farm = Farm.parse(input)

  override def part1(input: Farm): Long = {
    println(s"0: ${input.state}")
    val result = input.next(20)
    println(s"19: ${result.state}")

    result.value
  }

  override def part2(input: Farm): Long = input.next2(50000000000L).value
}

case class Farm(state: State, rules: Map[Pattern, Space]) {


  def value: Long = state.value

  def shiftBy(l: Long): Farm = {
    this.copy(state = this.state.shiftBy(l))
  }

  final def next2(generations: Long): Farm = {
    println(s"0: ${this.state}")
    val finalPattern = Stream.iterate((this, 0)) { case (farm, generation) =>
      (farm.next, generation + 1)
    }.dropWhile { case (farm, _) => !farm.state.samePattern(farm.next.state) }.head

    val r = finalPattern._1.shiftBy(generations - finalPattern._2)
    println(s"${finalPattern._2}: ${r.state}")
    r
  }


  @tailrec
  final def next(generations: Long): Farm = {
    assert(generations > 0)

    def innerNext(gens: Int): Farm = {
      val r = Range(0, gens).foldLeft(this) { case (farm, _) => {
        nextState(farm)
      } }
      r
    }

    if (generations > Int.MaxValue) {
      val leftOver = generations - Int.MaxValue
      innerNext(Int.MaxValue).next(leftOver)
    } else {
      innerNext(generations.toInt)
    }
  }

  def next: Farm = next(1)

  override def toString: String = s"$state\n${rules.toIndexedSeq.map(p => s"${p._1} -> ${p._2}").sorted.mkString("\n")}"
}

object Farm {

  private def nextState(farm: Farm): Farm = {
    val newState = farm.state.slideWith(farm.rules.withDefaultValue(Pot))
    farm.copy(state = newState)
  }

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
  def fromList(spaces: List[(Long, Space)]): Pattern = {
    require(spaces.size == 5, "Length of pattern must be 5")
    Pattern(spaces(0)._2, spaces(1)._2, spaces(2)._2, spaces(3)._2, spaces(4)._2)
  }
}

case class Rule(pattern: Pattern, result: Space) {
  override def toString: String = s"$pattern => $result"
}

class State private(state: List[(Long, Space)]) {

  private val spaces = state

  def shiftBy(l: Long): State = {
    new State(state.map { case (no, space) => (no + l, space)} )
  }


  def value: Long = spaces.filter(_._2 == Plant).map(_._1).sum

  def slideWith(f: Pattern => Space): State = {
    val result = spaces.sliding(5).foldLeft (List.empty[(Long, Space)]) { case (newSpaces, pattern) =>
      val spaceNo = pattern(2)._1
      val p = Pattern.fromList(pattern)
      val newSpace = (spaceNo, f(p))
      newSpace +: newSpaces
    }
    new State(withPadding(result.reverse))
  }

  def samePattern(other: State): Boolean = {
    spaces.map(_._2) == other.spaces.map(_._2)
  }

  override def toString: String = {
    spaces.head._1 + spaces.map(_._2).mkString("")
  }

  override def hashCode(): Int = spaces.hashCode()
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: State => other.spaces == this.spaces
      case _ => false
    }
  }

}

object State {

  private def withPadding(spaces: List[(Long, Space)]): List[(Long, Space)] = {
    val amountToPrepend = 5 - spaces.takeWhile(_._2 == Pot).length
    val amountToPostpend = 5 - spaces.reverse.takeWhile(_._2 == Pot).length

    val min = spaces.head._1
    val max = spaces.last._1
    val prefix = Range(-amountToPrepend, 0) map { i => (i + min, Pot) }
    val postfix = Range.inclusive(1, amountToPostpend) map { i => (i + max, Pot) }

    prefix.toList ++ spaces ++ postfix
  }


  def apply(spaces: IndexedSeq[Space]): State = {
    val indexed = spaces.zipWithIndex.map(_.swap).map { case (index, space) => (index.toLong, space)}.toList
    new State(withPadding(indexed))
  }
}


