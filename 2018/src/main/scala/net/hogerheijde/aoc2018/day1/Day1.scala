package net.hogerheijde.aoc2018.day1

import scala.collection.mutable

import net.hogerheijde.aoc.util.CircularBuffer
import net.hogerheijde.aoc.util.CircularBuffer.NonEmptyCircularBuffer
import net.hogerheijde.aoc.util.Parse
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day1.day1._


object Day1 extends Day2018[Model, Int, Int] {



  override def name: String = "Day 1"

  override def parse(in: String): Model = {
    Parse.standardLineSplit(in).map { drift =>
      val s = Sign(drift.take(1).toCharArray.head)
      val a = drift.drop(1).toInt
      Drift(s, a)
    }
  }

  override def part1(input: Model): Int = {
    input.foldLeft(0) { case (result, drift) =>
      result + drift.signed
    }

  }
  override def part2(input: Model): Int = {

    var buffer: NonEmptyCircularBuffer[Drift] = CircularBuffer(input: _*) match  {
      case necb: NonEmptyCircularBuffer[Drift] => necb
      case _ => throw new RuntimeException("Expect to have non-empty buffer")
    }

    var go = true
    var currentFrequency = 0
    val frequencies = mutable.Set(0)
    while(go) {
      currentFrequency = currentFrequency + buffer.current.asInstanceOf[Drift].signed
      buffer = buffer.rotate
      if (frequencies.contains(currentFrequency)) {
        go = false
      } else {
        frequencies.add(currentFrequency)
      }
    }

    currentFrequency
  }
}
