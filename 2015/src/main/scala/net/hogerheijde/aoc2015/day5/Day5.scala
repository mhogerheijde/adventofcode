package net.hogerheijde.aoc2015.day5

import net.hogerheijde.aoc2015.util.Day

import scala.collection.immutable.IndexedSeq

object Day5 extends Day[IndexedSeq[String], Int, Unit] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 5"
  override def parse: String => IndexedSeq[String] = input => input.lines.map(_.trim).toIndexedSeq


  override def part1(words: IndexedSeq[String]): Int = {
    val niceWords = words.map(judge) filter {
      case Nice => true
      case Naughty => false
    }

    niceWords.length
  }

  def judge(word: String): Judgement = {
    val isNice = Seq(containsAtLeast3Vowels, containsDoubles, containsNoIllegalCombinations).forall(rule => rule(word))
    Judgement(isNice)
  }

  override def part2(input: IndexedSeq[String]): Unit = ()


  private val vowels: Set[Char] = "aeiou".toSet
  val containsAtLeast3Vowels: String => Boolean = { input =>
    input.foldLeft(0) { (total, char) => if (vowels.contains(char)) { total + 1 } else { total } } >= 3
  }

  val containsDoubles: String => Boolean = { input =>
    input.sliding(2).exists(chars => chars.distinct.length == 1)
  }


  val illegalCombinations = Set("ab", "cd", "pq", "xy")
  val containsNoIllegalCombinations: String => Boolean = { input =>
    input.sliding(2).map(_.mkString("")).forall(part => !illegalCombinations.contains(part))
  }


}
