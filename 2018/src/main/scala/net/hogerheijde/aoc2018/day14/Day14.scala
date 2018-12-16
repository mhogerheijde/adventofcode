package net.hogerheijde.aoc2018.day14

import net.hogerheijde.aoc2018.Day2018

import scala.collection.immutable.IndexedSeq

object Day14 extends Day2018[Int, String, Int] {

  override def parse(input: String): Int = input.trim.toInt

  override def part1(input: Int): String = {
   Recipies.initialState.findPart1Score(input)
  }

  override def part2(input: Int): Int = {
    Recipies.initialState.findPart2Score(input.toString)
  }
}

class Recipies private (
    val scores: IndexedSeq[Int],
    val elf_1: Int,
    val elf_2: Int,
    val length: Int) {

  def findPart2Score(i: String): Int = {

    val needle = i.map(_.asDigit).reverse

    val result = Stream.iterate(this)(_.next).dropWhile(r => {
      val match1 = r.scores.take(5)
      val match2 = r.scores.drop(1).take(5)
      needle != match1 && needle != match2
    }).head

    if (result.scores.take(5) == needle) {
      result.scores.drop(5).length
    } else if (result.scores.drop(1).take(5) == needle) {
      result.scores.drop(6).length
    } else {
      throw new RuntimeException("Needle not found in resulting recipie list?!")
    }
  }

  def findPart1Score(amount: Int): String = {
    val recipies = makeRecipies(amount + 10)
    val tooMany = recipies.scores.length - amount - 10
    recipies.scores.drop(tooMany).take(10).reverse.mkString("")
  }

  def makeRecipies(amount: Int): Recipies = {
    Stream.iterate(this)(_.next).dropWhile(_.length < amount).head
  }

  def next(generations: Int): Recipies = Stream.iterate(this)(_.next).drop(generations).head

  def next: Recipies = {
    val current1 = scores(length - 1 - elf_1)
    val current2 = scores(length - 1 - elf_2)

    val newRecipieScores = current1 + current2

    val newScores = if (newRecipieScores < 10) {
      newRecipieScores +: scores
    } else {
      val tens = newRecipieScores / 10
      val ones = newRecipieScores % 10
      ones +: (tens +: scores)
    }

    val newLength = newScores.length
    val newElf1 = nextPosition(elf_1, current1, newLength)
    val newElf2 = nextPosition(elf_2, current2, newLength)

    new Recipies(newScores, newElf1, newElf2, newLength)
  }

  def nextPosition(currentPos: Int, currentScore: Int, length: Int): Int = {
    val nextPos = currentPos + ((currentScore + 1) % length)
    if (nextPos >= length) { nextPos - length } else { nextPos }
  }


  override def toString: String = {
    val b = new StringBuilder
    scores.reverse.zipWithIndex.foreach { case (score, index) =>
      if (index == elf_1) {
        b append s"($score) "
      } else if (index == elf_2) {
        b append s"[$score] "
      } else {
        b append s"$score "
      }
    }
    b.toString
  }

  override def hashCode(): Int = scores.hashCode() + elf_1.hashCode() + elf_2.hashCode() + length.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Recipies =>
        this.scores == other.scores && this.elf_1 == other.elf_1 && this.elf_2 == other.elf_2 && this.length == other.length
      case _ => false
    }
  }

}

object Recipies {
  def apply(scores: IndexedSeq[Int], elf_1: Int, elf_2: Int): Recipies =
    new Recipies(scores, elf_1, elf_2, scores.length)
  
  def initialState: Recipies = new Recipies(IndexedSeq(7, 3), 0, 1, 2)
}
