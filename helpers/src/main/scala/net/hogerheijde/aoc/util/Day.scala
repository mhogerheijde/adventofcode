package net.hogerheijde.aoc.util

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import net.hogerheijde.aoc.util.Timer.TimedResult

/***
  *
  * @tparam Model The model type for this day. Represents the puzzle-input after parsing
  * @tparam Result1 The result type of part 1
  * @tparam Result2 The result type of part 2
  */
trait Day[Model, Result1, Result2] extends Runnable {

  def year: String

  /**
    * The name of the day
    * @return
    */
  def name: String

  /**
    * Parses the string of the puzzle input to the model type
    * @return
    */
  def parse(input: String): Model

  def part1(input: Model): Result1
  def part2(input: Model): Result2

  final def main(args: Array[String]): Unit = run
  final def run: Unit = {
    val input = {
      val resourceName = s"net/hogerheijde/$year/${name.toLowerCase.replace(" ", "")}.input"
      Try {
        val source = Source.fromResource(resourceName)
        source.mkString.trim
      } match {
        case Success(string) => string
        case Failure(t) => throw new RuntimeException(s"Did you forget to place the puzzle input in the resources folder? Trying to read $resourceName", t)
      }
    }

    println(s"$name:")
    val TimedResult(parsedInput, parseTime) = Timer(parse(input))
    println(s"Parsing: $parseTime")

    val result1 = Timer(part1(parsedInput))
    println(s" - part 1: $result1")
    val result2 = Timer(part2(parsedInput))
    println(s" - part 2: $result2")
  }
}
