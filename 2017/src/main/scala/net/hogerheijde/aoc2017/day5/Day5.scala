package net.hogerheijde.aoc2017.day5

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2017.Day2017

object Day5 extends Day2017[IndexedSeq[Int], Int, Int]{
  override def name: String = "Day 5"
  override def parse(input: String): IndexedSeq[Int] = input.lines.map(Integer.parseInt).toIndexedSeq

  override def part1(input: IndexedSeq[Int]): Int = {
    val state = State(input)
    val result = resolve(state)

    result.iteration
  }

  override def part2(input: IndexedSeq[Int]): Int = {
    val state = State(input)
    val result = resolve2(state)

    result.iteration
  }

  @tailrec
  def resolve(state: State): State = {
    state match {
      case e: Ended => e
      case o =>
        val next = nextState(state)
        resolve(next)
    }
  }

  @tailrec
  def resolve2(state: State): State = {
    state match {
      case e: Ended => e
      case o =>
        val next = nextState2(state)
        resolve2(next)
    }
  }

  def nextState(current: State): State = {
    current match {
      case e: Ended => e
      case running: Running =>
        val instruction = running.instructions.get(running.pc)
        instruction match {
          case None => Ended(running.iteration)
          case Some(jump) =>
            val nextPc = running.pc + jump
            State(
              instructions = running.instructions.updated(running.pc, jump+1),
              pc = nextPc,
              iteration = running.iteration + 1)
        }
    }
  }

  def nextState2(current: State): State = {
    current match {
      case e: Ended => e
      case running: Running =>
        val instruction = running.instructions.get(running.pc)
        instruction match {
          case None => Ended(running.iteration)
          case Some(jump) =>
            val nextPc = running.pc + jump
            if (jump >= 3) {
              State(
                instructions = running.instructions.updated(running.pc, jump - 1),
                pc = nextPc,
                iteration = running.iteration + 1)
            } else {
              State(
                instructions = running.instructions.updated(running.pc, jump + 1),
                pc = nextPc,
                iteration = running.iteration + 1)
            }
        }
    }
  }

}
