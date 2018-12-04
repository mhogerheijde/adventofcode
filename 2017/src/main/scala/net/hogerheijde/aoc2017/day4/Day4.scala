package net.hogerheijde.aoc2017.day4

import scala.collection.immutable
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2017.Day2017

object Day4 extends Day2017[IndexedSeq[String], Int, Int]{
  override def name: String = "Day 4"
  override def parse(input: String): IndexedSeq[String] = input.trim.lines.toIndexedSeq

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
