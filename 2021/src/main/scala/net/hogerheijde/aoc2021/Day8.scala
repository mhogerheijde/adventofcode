package net.hogerheijde.aoc2021

import fastparse.CharIn
import fastparse.P
import fastparse._
//import fastparse.SingleLineWhitespace._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day8 extends Day[Int, Int] {
  override type Model = Seq[Note]

  case class Display(segments: Set[Char]) {
    // Display characters for 1, 4, 7 and 8 have a unique segment count
    val unique = Set(2, 3, 4, 7).contains(segments.size)
  }
  object Display {
    def apply(segments: String): Display = Display(segments.toSet)
  }
  case class Monitor(
      d1: Display,
      d2: Display,
      d3: Display,
      d4: Display,
  ) {
    private val displays = Seq(d1, d2, d3, d4)
    val countUnique = displays.count(_.unique)
  }
  object Monitor {
    def apply(values: Seq[Display]): Monitor = {
      require(values.length == 4, "Must initialise Monitor with exactly 4 displays")
      Monitor(values(0), values(1), values(2), values(3))
    }
  }
  case class Note(pattern: Seq[Display], readout: Monitor) {

    def resolveDigits: Map[Display, Int] = {
      // be bold and just get
      val one = pattern.find(_.segments.size == 2).get
      val four = pattern.find(_.segments.size == 4).get
      val seven = pattern.find(_.segments.size == 3).get
      val eight = pattern.find(_.segments.size == 7).get

      val three = pattern.find(d =>
        // only 2, 3, 5 have 5 segments, but only 3 shares all of 1s segments
        d.segments.size == 5 &&
          one.segments.subsetOf(d.segments)
      ).get

      val nine = pattern.find(d =>
        // only 0, 6, 9 have 6 segments, but only 9 shares all of 3s segments
        d.segments.size == 6 &&
          three.segments.subsetOf(d.segments)
      ).get

      val zero = pattern.find(d =>
        // only 0, 6, 9 have 6 segments, but only 9 shares all of 3s segments
        // We found 9. so only 0 & 6 left with 6 segments.
        // 0 does share segments with 1, 6 doesn't.
        d.segments.size == 6 &&
          d.segments != nine.segments &&
          one.segments.subsetOf(d.segments)
      ).get

      val six = pattern.find(d =>
        // only 0, 6, 9 have 6 segments, but only 9 shares all of 3s segments
        // We found 9. so only 0 & 6 left with 6 segments.
        // 0 does share segments with 1, 6 doesn't.
        d.segments.size == 6 &&
          d.segments != nine.segments &&
          d.segments != zero.segments
      ).get

      val five = pattern.find(d =>
        // only 2, 3, 5 have 5 segments, we found 3
        // 5 is a subset of 9, 2 is not
        d.segments.size == 5 &&
          d.segments != three.segments &&
          d.segments.subsetOf(nine.segments)
      ).get

      val two = pattern.find(d =>
        // only 2, 3, 5 have 5 segments, we found 3 and 5
        d.segments.size == 5 &&
          d.segments != three.segments &&
          d.segments != five.segments
      ).get

      Map(
        zero -> 0,
        one -> 1,
        two -> 2,
        three -> 3,
        four -> 4,
        five -> 5,
        six -> 6,
        seven -> 7,
        eight -> 8,
        nine -> 9,
      )
    }

    def resolveValue: Int = {
      val resolved = resolveDigits
      resolved(readout.d1) * 1000 +
        resolved(readout.d2) * 100 +
        resolved(readout.d3) * 10 +
        resolved(readout.d4)
    }
  }

  def notes[_: P]: P[Seq[Note]] = P((note ~ "\n".?).rep)
  def note[_: P]: P[Note] = P(pattern ~ "| " ~ display ~ " " ~ display ~ " " ~ display ~ " " ~ display).map {
    case (pattern, d1, d2, d3, d4) => Note(pattern, Monitor(d1, d2, d3, d4))
  }

  def pattern[_: P]: P[Seq[Display]] = P((display ~ " ").rep)
  def display[_: P]: P[Display] = P(CharIn("abcdefg").rep.!).map { chars => Display(chars.toSet) }

  override def parse(input: String): Model = Parser.parse(notes(_))(input).get

  override def part1(input: Model): Int = input.map(_.readout.countUnique).sum
  override def part2(input: Model): Int = input.map(_.resolveValue).sum
}
