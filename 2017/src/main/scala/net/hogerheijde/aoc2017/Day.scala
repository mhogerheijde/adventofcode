package net.hogerheijde.aoc2017

import scala.io.Source

/***
  *
  * @tparam Model The model type for this day. Represents the puzzle-input after parsing
  * @tparam Result1 The result type of part 1
  * @tparam Result2 The result type of part 2
  */
trait Day[Model, Result1, Result2] {

  /**
    * The name of the day
    * @return
    */
  def name: String

  /**
    * Parses the string of the puzzle input to the model type
    * @return
    */
  def parse: String => Model

  def part1(input: Model): Result1
  def part2(input: Model): Result2

  def run(): Unit = {
    val input = {
      Option(Source.fromResource(s"net/hogerheijde/aoc2017/${name.toLowerCase.replace(" ", "")}.input")) match {
        case Some(source) => source.mkString.trim
        case None => throw new RuntimeException("Did you forget to place the puzzle input in the resources folder?")
      }

    }

    println(s"$name:")
    val boxes = parse(input)


    val result1 = part1(boxes)
    println(s" - part 1: $result1")
    val result2 = part2(boxes)
    println(s" - part 2: $result2")
  }
}
