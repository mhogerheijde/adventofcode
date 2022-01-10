package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.util.Day
object Day10 extends Day[Int, Long] {
  type Model = Seq[(String, List[(Char, Char)], List[Char])]

  override def parse(input: String): Model = {
    input.linesIterator.toSeq.map(syntax)
  }

  val pairs = Map(
    '(' -> ')',
    '<' -> '>',
    '{' -> '}',
    '[' -> ']',
  )
  val openChars = pairs.keys.toSet
  val closeChars = pairs.values.toSet
  val syntaxScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )
  val completeScore = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )

  def syntax(line: String): (String, List[(Char, Char)], List[Char]) = {
    val (stack, errors) = line.foldLeft((List.empty[Char], List.empty[(Char, Char)])) { case ((stack, errors), next) =>
      if (openChars.contains(next)) {
        // new chunk, push expected close char on the stack
        (pairs(next) :: stack, errors)
      } else if (
        closeChars.contains(next) &&
          stack.headOption.contains(next)
      ) {
        // closing char and we expect it
        (stack.tail, errors)
      } else if (
        closeChars.contains(next) &&
          !stack.headOption.contains(next)
      ) {
        // closing char and we did not expect it!
        (stack.tail, errors :+ (stack.head, next))
      } else {
        throw new RuntimeException("This should not occur I think?")
      }
    }
    (line, errors, stack)
  }

  override def part1(input: Model): Int = {
    input.flatMap(_._2.headOption.map(error => syntaxScore(error._2))).sum
  }

  override def part2(input: Model): Long = {
    val scores = input.filter(q => q._2.isEmpty).map { syntax =>
      val s = syntax._3.foldLeft(0L) { case (score, nextChar) =>
        (score * 5) + completeScore(nextChar)
      }
      (syntax._3.mkString, s) // keep s for debugging purposes
    }.sortBy(_._2)
    scores.map(_._2)(scores.size / 2)
  }
}
