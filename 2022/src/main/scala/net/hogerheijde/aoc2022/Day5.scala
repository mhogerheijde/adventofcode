package net.hogerheijde.aoc2022

import scala.annotation.tailrec

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day5 extends Day[String, String] {

  override type Model = State

  type CargoStack = List[Char]
  val CargoStack = List

  case class Move(amount: Int, from: Int, to: Int) {
    val fromIdx = from - 1
    val toIdx = to - 1
    override def toString: String = s"move $amount from $from to $to"
  }
  
  case class State private(
      stacks: IndexedSeq[CargoStack],
      moves: List[Move],
  ) {

    val topLine: String = stacks.flatMap(_.headOption).mkString("")
    
    
    
    def step(strategy: Strategy): Option[State] = moves match {
      case move +: tail => Some(State(strategy(stacks, move), tail))
      case _ => None
    }
    
    @tailrec
    final def solve(strategy: Strategy): State = this.step(strategy) match {
      case None => this
      case Some(newState) => newState.solve(strategy)
    }
    
    override def toString: String = {
      val m = stacks.maxBy { _.length }.length
      val t = stacks.map { s =>
        val padding = (" " * (m - s.length)).toSeq.map(_.toChar)
        s.toSeq.prependedAll(padding) 
      }.transpose
      t.map { line =>
        line.mkString("[", "] [", "]")
      }.mkString("\n") + "\n\n" + moves.map(_.toString).mkString("\n")
    }
  }

  type Strategy = (IndexedSeq[CargoStack], Move) => IndexedSeq[CargoStack]
  
  def strategy1: Strategy = { case(s, move) =>
    val (toMove, newFrom) = s(move.fromIdx).splitAt(move.amount)
    val newTo = s(move.toIdx).prependedAll(toMove.reverse)

    s
      .updated(move.fromIdx, newFrom)
      .updated(move.toIdx, newTo)
  }

  def strategy2: Strategy = {
    case (s, move) =>
      val (toMove, newFrom) = s(move.fromIdx).splitAt(move.amount)
      val newTo = s(move.toIdx).prependedAll(toMove)

      s
        .updated(move.fromIdx, newFrom)
        .updated(move.toIdx, newTo)
  }

  override def parse(input: String): Day5.Model = Parser.parse(state(_))(input).get

  override def part1(input: Day5.Model): String = input.solve(strategy1).topLine

  override def part2(input: Day5.Model): String = input.solve(strategy2).topLine

  // Parsers & helpers
  def emptyStackItem[_: P]: P[Option[Char]] = P("    " | "   ").map(_ => None)
  def filledStackItem[_: P]: P[Option[Char]] = P("[" ~ SingleChar ~ "]" ~ " ".?).map(Some(_))
  def stackItem[_: P]: P[Option[Char]] = P(emptyStackItem | filledStackItem)
  def stackLine[_: P]: P[Seq[Option[Char]]] = P(stackItem.rep ~ "\n")
  def stackNumbers[_: P]: P[Seq[Int]] = P((" " ~ int ~ " " ~ " ".?).rep ~ "\n")
  def stackLines[_: P]: P[Seq[Seq[Option[Char]]]] = P(stackLine.rep)
 
  def stack[_: P]: P[IndexedSeq[CargoStack]] = P(stackLine.rep ~ stackNumbers).map { case (stack, _) =>
    val transposed: Seq[Seq[Option[Char]]] = stack.transpose
    transposed.toIndexedSeq.map { s => CargoStack(s.flatten: _*) }
  }

  def move[_: P]: P[Move] = P("move " ~ int ~ " from " ~ int ~ " to " ~ int ~ "\n").map { case (amount, from, to) => Move(amount, from, to) }
  def moves[_: P]: P[List[Move]] = P(move.rep).map(_.toList)
  
  def state[_: P]: P[State] = P(stack ~ "\n" ~ moves).map { case (s, m) => State(s, m)  
  }
}


