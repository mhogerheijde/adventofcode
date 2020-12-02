package net.hogerheijde.aoc2020

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.util.Implicits.BooleanHelper

object Day2 extends Day[Int, Int] {
  case class PasswordRule(min: Int, max: Int, char: Char, password: String)
  override type Model = Seq[PasswordRule]

  override def parse(input: String): Model = {
    input.linesIterator.flatMap { line =>
      Parser.parse(password(_))(line)
    }.map { case (min, max, char, password) =>
      PasswordRule(min, max, char, password)
    }.toSeq
  }

  override def part1(input: Model): Int = input.count(isValidDay1)
  override def part2(input: Model): Int = input.count(isValidDay2)

  def isValidDay1(rule: PasswordRule): Boolean = {
    rule.password.count(_ == rule.char).isBetweenInclusive(rule.min, rule.max)
  }

  def isValidDay2(rule: PasswordRule): Boolean = {
    val first = rule.password.charAt(rule.min - 1)
    val second = rule.password.charAt(rule.max - 1)

    val result = (first == rule.char) xor (second == rule.char)
    result
  }

  implicit class IntHelper(i: Int) {
    def isBetweenInclusive(min: Int, max: Int): Boolean = {
      i >= min && i <= max
    }
  }

  def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
  def password[_: P]: P[(Int, Int, Char, String)] = {
    P(Start ~ number ~ "-" ~ number ~ " " ~ AnyChar.!.map(_.toCharArray.head) ~ ": " ~ AnyChar.rep(1).! ~ End)
  }

}
