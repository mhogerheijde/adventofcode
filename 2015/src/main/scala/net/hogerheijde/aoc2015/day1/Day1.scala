package net.hogerheijde.aoc2015.day1

import net.hogerheijde.aoc2015.util.Day

import scala.io.Source


/**
  * http://adventofcode.com/2015/day/1
  *
  * Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the
  * directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions
  * one character at a time.
  *
  * An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go
  * down one floor.
  *
  * The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
  *
  * For example:
  *
  *  (()) and ()() both result in floor 0.
  *  ((( and (()(()( both result in floor 3.
  *  ))((((( also results in floor 3.
  *  ()) and ))( both result in floor -1 (the first basement level).
  *  ))) and )())()) both result in floor -3.
  *
  * --- Part Two ---
  *
  * Now, given the same instructions, find the position of the first character that causes him to enter the basement
  * (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.
  *
  * For example:
  *
  *   ) causes him to enter the basement at character position 1.
  *   ()()) causes him to enter the basement at character position 5.
  *
  * What is the position of the character that causes Santa to first enter the basement?
  */
object Day1 extends Day[String, Int, Option[Int]]{
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 1"
  override def parse: String => String = identity


  def part1(input: String): Int = {
    input.foldLeft(0) { (total, currentChar) =>
      currentChar match {
        case '(' => total + 1
        case ')' => total - 1
      }
    }
  }

  def part2(input: String): Option[Int] = {
    input.zipWithIndex.foldLeft((Option.empty[Int], 0)) { case ((found, currentCount), (currentChar, currentPos)) =>
      found match {
        case None =>
          val newCount = currentChar match {
            case '(' => currentCount + 1
            case ')' => currentCount - 1
          }

          if (newCount == -1) {
            (Some(currentPos + 1), newCount)
          } else {
            (found, newCount)
          }
        case _ => (found, currentCount)
      }
    }._1
  }


}
