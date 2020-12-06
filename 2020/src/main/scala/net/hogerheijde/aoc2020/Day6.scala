package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Day

object Day6 extends Day[Int, Int] {

  type Answers = Set[Char]
  type Group = Seq[Answers]
  type Model = Seq[Group]

  override def parse(input: String): Day6.Model = input
    .linesIterator
    .foldLeft(Seq(Seq.empty[Answers])) { case (acc, nextLine) =>
      nextLine match {
        case "" => acc :+ Seq.empty[Answers]
        case chars => acc.init :+ (acc.last :+ chars.toSet)
      }
    }

  override def part1(input: Day6.Model): Int = input
    .map(_.flatten.toSet)
    .map(_.size)
    .sum

  override def part2(input: Day6.Model): Int = input
    .map { group =>
      // Histogram per group
      group
        .foldLeft(Map.empty[Char, Int].withDefaultValue(0)) { case (acc, nextAnswers) =>
          nextAnswers.foldLeft(acc) { case (subAcc, nextSingleAnswer) =>
            subAcc.updated(nextSingleAnswer, subAcc(nextSingleAnswer) + 1)
          }
        }
        .filter { case (_, hist) => hist == group.size }
    }
    .map(_.size)
    .sum
}
