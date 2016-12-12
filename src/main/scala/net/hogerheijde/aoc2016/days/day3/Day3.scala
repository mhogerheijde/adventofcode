package net.hogerheijde.aoc2016.days.day3

import net.hogerheijde.aoc2016.days.day3.model.Triangle

import scala.util.Try

object Day3 {


  def process(input: String): IndexedSeq[Triangle] = {
    val sidess = parse(input)
    sidess.flatMap(buildTriangle)
  }

  def parse(input: String): IndexedSeq[IndexedSeq[Int]] = {
    input.trim().split("\n").map(parseLine)
  }

  def parseLine(line: String): IndexedSeq[Int] = {
    line.split(" +").flatMap(side => Try(side.toInt).toOption).toIndexedSeq
  }

  def buildTriangle(sides: IndexedSeq[Int]): Option[Triangle] = {
    sides.sorted match {
      case IndexedSeq(one, two, three) =>
        if (one + two > three) {
          Some(Triangle(one, two, three))
        } else {
          None
        }
      case _ => None
    }

  }

}
