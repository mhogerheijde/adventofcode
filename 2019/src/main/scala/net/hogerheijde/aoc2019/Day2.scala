package net.hogerheijde.aoc2019

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.util.Day

object Day2 extends Day[Int, Int] {
  type Model = IntComputer

  override def name: String = "Day 2"

  override def parse(in: String): Model = {
    val programMemory = in.split(",").map(_.toInt).toIndexedSeq
    IntComputer(programMemory)
  }

  override def part1(computer: Model): Int = {
    val result = computer.calculate(12, 2)
    result(0)
  }


  override def part2(computer: Model): Int = {
    val output = (for {
      noun <- Range(0, 99).inclusive
      verb <- Range(0, 99).inclusive
    } yield {
      (noun, verb, computer.calculate(noun, verb))
    })
      .find( x => x._3(0) == 19690720)
      .get

    (100 * output._1) + output._2
  }
}
