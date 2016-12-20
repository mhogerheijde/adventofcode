package net.hogerheijde.aoc2016.days.day2

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay
import net.hogerheijde.aoc2016.days.day2.model.Instruction
import net.hogerheijde.aoc2016.days.day2.model.Key
import net.hogerheijde.aoc2016.days.day2.model.KeyPadSquare
import net.hogerheijde.aoc2016.days.day2.model.KeyPadStar

import scala.collection.immutable.IndexedSeq

object Day2 extends RunnableDay {

  override def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day2.input")
    val code = Day2.process(input, KeyPadSquare.Five)
    println(s"Day 02 - pt1: $code (expect 53255)")
    val code2 = Day2.process(input, KeyPadStar.Five)
    println(s"Day 02 - pt2: $code2 (expect 7423A)")
  }

  type Instructions = IndexedSeq[IndexedSeq[Instruction]]
  type Code = IndexedSeq[Key]
  type CodeWithHistory = IndexedSeq[(Key, IndexedSeq[Key])]

  def process(input: String, defaultKey: Key): String = {
    val instructions = parseInstructions(input)
    val code = processInstructions(instructions, defaultKey)
    code.map(_.prettyString()).mkString("")
  }

  def processInstructions(instructions: Instructions, defaultKey: Key): Code = {
    val keys: Code = IndexedSeq()
    instructions.foldLeft( keys ) { case (accKeys, instructionLine) =>
      val startKey = accKeys.lastOption.getOrElse(defaultKey)
      val newKey = instructionLine.foldLeft(startKey) { case (key, nextInstruction) =>
          key.update(nextInstruction)
      }
      accKeys :+ newKey
    }
  }

  def parseInstructions(input: String): Instructions = {
    input.split("\n").toIndexedSeq map { line =>
      line.takeWhile(_ != null).map(x => Instruction.fromChar(x)).flatten
    }
  }


}
