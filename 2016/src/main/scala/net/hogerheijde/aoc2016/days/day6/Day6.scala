package net.hogerheijde.aoc2016.days.day6

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay

import scala.collection.immutable.IndexedSeq

object Day6 extends RunnableDay {

  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day6.input")

    val fixed = Day6.process(input)
    println(s"Day 06 - pt1: $fixed (expect dzqckwsd)")
    val fixed2 = Day6.process2(input)
    println(s"Day 06 - pt2: $fixed2 (expect lragovly)")
  }

  type Histogram = IndexedSeq[Map[Char, Int]]
  val Histogram = scala.collection.immutable.IndexedSeq

  def process(input: String): String = {
    val words = parse(input)
    val hist = histogram(words)
    errorCorrect(hist)
  }

  def process2(input: String): String = {
    val words = parse(input)
    val hist = histogram(words)
    errorCorrect2(hist)
  }

  def parse(input: String): IndexedSeq[String] = input.split("\n").toIndexedSeq

  def errorCorrect(histogram: Histogram): String = {
    val x = histogram map { positionalHistogram =>
      val start: (Char, Int) = (' ', -1)

      positionalHistogram.foldLeft( start ) { case ((candidateChar, candidateFrequency), (char, frequency)) =>
        if (candidateFrequency > frequency) {
          (candidateChar, candidateFrequency)
        } else {
          (char, frequency)
        }
      }
    }
    x.map(_._1).mkString
  }

  def errorCorrect2(histogram: Histogram): String = {
    val x = histogram map { positionalHistogram =>
      val start: (Char, Long) = (' ', Long.MaxValue)

      positionalHistogram.foldLeft( start ) { case ((candidateChar, candidateFrequency), (char, frequency)) =>
        if (candidateFrequency < frequency) {
          (candidateChar, candidateFrequency)
        } else {
          (char, frequency)
        }
      }
    }
    x.map(_._1).mkString
  }

  def histogram(words: IndexedSeq[String]): Histogram = {
    val emptyHistogram = Histogram[Map[Char, Int]]()
    words.foldLeft(emptyHistogram) { case (accHistogram, nextWord) =>
      nextWord.zipWithIndex.foldLeft(accHistogram) { case (accHist, (char, index)) =>
        accHist.lift(index) match {
          case Some(map) => accHist.updated(index, map.updated(char, map(char) + 1))
          case None =>
            // Assume that if the index doesn't exist yet, it is the _next_ index
            // FIXME figure out how to make sure we're updating the correct position.
            accHist :+ (Map(char -> 1).withDefaultValue(0))
        }

      }
    }
  }

}
