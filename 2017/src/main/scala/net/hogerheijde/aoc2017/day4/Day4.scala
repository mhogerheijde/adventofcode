package net.hogerheijde.aoc2017.day4

import net.hogerheijde.aoc2017.Day

import scala.collection.immutable
import scala.collection.immutable.IndexedSeq

object Day4 extends Day[IndexedSeq[String], Int, Int]{
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 4"
  override def parse: String => IndexedSeq[String] = input => input.trim.lines.toIndexedSeq

  override def part1(input: IndexedSeq[String]): Int = {
    input.foldLeft(0) { (validPhrases, phrase) =>
      val parts = phrase.split(" ").toIndexedSeq
      if (parts.length == parts.distinct.length) { validPhrases + 1 } else { validPhrases }
    }
  }

  override def part2(input: IndexedSeq[String]): Int = input.foldLeft(0) { (validPhrases, phrase) =>
    val parts: immutable.Seq[Set[Char]] = phrase.split(" ").toIndexedSeq.map(_.toSet)
    if (parts.length == parts.distinct.length) { validPhrases + 1 } else { validPhrases }
  }
}
