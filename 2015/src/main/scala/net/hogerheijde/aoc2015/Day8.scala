package net.hogerheijde.aoc2015

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc2015.util.Day
import net.hogerheijde.aoc.util.Parser

object Day8 extends Day[Int, Int] {

  type Model = Seq[String]

  override def name: String = "Day 8"

  override def parse: String => Day8.Model = input => input
    .linesIterator
    .toSeq

  override def part1(input:  Day8.Model): Int = {
    input
      .map(line => (line, Parser.parse(inMemory(_))(line).get))
      .foldLeft(0) { case (acc, next) =>
        acc + next._1.length - next._2.size
      }
  }

  override def part2(input:  Day8.Model): Int = {
    input
      .map { line =>
        val escaped = "\"" + line.map {
          case '"' => "\\\""
          case '\\' => "\\\\"
          case c => c
        }.mkString + "\""

        (escaped, line)
      }
      .foldLeft(0) { case (acc, next) =>
        acc + next._1.length - next._2.length
      }
  }

  sealed trait Token { def value: String }
  case class TokenLiteral(value: String) extends Token
  case class TokenHex(value: String) extends Token
  def escapedHex[_: P]: P[Token] = P("\\x" ~ CharIn("0-9a-f").rep(min = 2, max=2).!).map(TokenHex(_))
  def escapedOther[_: P]: P[Token] = P("\\" ~ ("\\" | "\"").!).map(TokenLiteral(_))
  def escaped[_: P]: P[Token] = P(escapedHex | escapedOther)
  def token[_: P]: P[Token] = P(CharIn("a-z").!).map(TokenLiteral(_))
  def tokens[_: P]: P[Seq[Token]] = P((token | escaped).rep)
  def inMemory[_: P]: P[Seq[Token]] = P(Start ~ "\"" ~ tokens ~ "\"" ~ End)

}
