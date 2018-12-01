package net.hogerheijde.aoc2017.day6

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2017.Day2017

object Day6 extends Day2017[State, Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 6"

  override def parse: String => State = { input =>
    State.from(input.trim.split("\t").map(Integer.parseInt).toIndexedSeq)
  }

  override def part1(input: State): Int = {
    val result = resolve(0, IndexedSeq(), input)
    result._1
  }

  override def part2(input: State): Int = {
    val result = resolve(0, IndexedSeq(), input)
    result._1 - result._2.indexOf(result._3)
  }


  def nextState1(currentState: State): State = {
    val registerOfInterest = max(currentState)
    val value = registerOfInterest._2

    val numberOfRegisters = currentState.size
    val increment = Math.floor(value / numberOfRegisters).toInt
    val left = value % numberOfRegisters

    State(currentState.state.map { register =>
      if (register == registerOfInterest) {
        (register._1, increment)
      } else {
        val offset = (numberOfRegisters + (register._1 - registerOfInterest._1)) % numberOfRegisters
        val extra = if (offset <= left) { 1 } else { 0 }
        (register._1, register._2 + increment + extra)
      }
    })
  }

  @tailrec
  def resolve(iteration: Int, pastStates: IndexedSeq[State], currentState: State): (Int, IndexedSeq[State], State) = {
    if(pastStates.contains(currentState)) {
      (iteration, pastStates, currentState)
    } else {
      val next: State = nextState1(currentState)
      resolve(iteration + 1, pastStates :+ currentState, next)
    }
  }


  def max(state: State): Register = {
    val highestHeldRegisterValue = state.maxRegister
    val allRegisterWithHighestValue = state.state.filter { case (_, value) => value == highestHeldRegisterValue }
    val lowestRegister = allRegisterWithHighestValue.keys.min
    val lowestRegisterWithHighestValue = state.state.find(_._1 == lowestRegister).get
    lowestRegisterWithHighestValue
  }

}
