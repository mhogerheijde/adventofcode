package net.hogerheijde.aoc2019.day2

import scala.annotation.tailrec

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2019.Day2019


object Day2 extends Day2019[Model, Int, Int] {

  override def name: String = "Day 2"

  override def parse(in: String): Model = {
    in.split(",").map(_.toInt).toIndexedSeq
  }

  override def part1(input: Model): Int = {
    val actualInput = input.updated(1, 12).updated(2, 2)
    val result = iterate(0, actualInput)

    result(0)


  }


  override def part2(input: Model): Int = {
    0
  }

  @tailrec
  def iterate(pc: Int, state: Model): Model = {
    state(pc) match {
      case 1 => {
        val newValue = state(state(pc + 1)) + state(state(pc + 2))
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState)
      }
      case 2 => {
        val newValue = state(state(pc + 1)) * state(state(pc + 2))
        val newState = state.updated(state(pc + 3), newValue)
        iterate(pc + 4, newState)
      }
      case 99 => state
    }
  }
}
