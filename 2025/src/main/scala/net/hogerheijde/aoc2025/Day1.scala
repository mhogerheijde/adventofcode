package net.hogerheijde.aoc2025

import net.hogerheijde.aoc.util.Day
import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser

object Day1 extends Day[Int, Int]:
  type Model = Seq[Instruction]

  case class Dial(position: Int, size: Int):
    require (size > 0, "Dial size must be positive")
    def rotate(instruction: Instruction): (Dial, Int) = {
      instruction match
        case Instruction.Left(steps) =>
          val newPosition = (position - (steps % size) + size) % size
          val atZero = steps / size + (if (position != 0 && (newPosition == 0 || newPosition > position )) then 1 else 0)
          (copy(position = newPosition), atZero)
        case Instruction.Right(steps) =>
          val newPosition = (position + steps) % size
          val pastZero = steps / size + (if (position != 0 && (newPosition == 0 || newPosition < position )) then 1 else 0)
          (copy(position = newPosition), pastZero)
    }

  enum Instruction:
    val steps: Int

    case Left(steps: Int)
    case Right(steps: Int)

  override def parse(input: String): Model =
    Parser.parse(instructions(_))(input).get

  override def part1(input: Model): Int = {
    val x = input.foldLeft((0, Dial(50, 100))) { case ((count, dial), next) =>
      val updated = dial.rotate(next)
      (count + (if updated._1.position == 0 then 1 else 0) , updated._1)
    }
    x._1
  }

  override def part2(input: Model): Int = {
    val x = input.foldLeft((0, Dial(50, 100))) { case ((count, dial), next) =>
      val updated = dial.rotate(next)
      (count + updated._2, updated._1)
    }
    x._1
  }

  def instructions[$: P]: P[Seq[Instruction]] = ((left | right) ~ "\n").rep()

  def left[$: P]: P[Instruction.Left] = P("L" ~ int).map(Instruction.Left(_))
  def right[$: P]: P[Instruction.Right] = P("R" ~ int).map(Instruction.Right(_))
