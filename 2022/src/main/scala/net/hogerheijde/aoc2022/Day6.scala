package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.util.Day

object Day6 extends Day[Int, Int] {
  override type Model = Seq[Char]
  override def parse(input: String): Model = input.toSeq

  
  def findMarker(input: Model, windowSize: Int): Int = {
    val marker = input.sliding(windowSize).zipWithIndex.find { case (quadrupel, _) =>
      quadrupel.toSet.size == windowSize
    }
    marker.get._2 + windowSize
  } 
  
  override def part1(input: Model): Int = findMarker(input, 4)
  override def part2(input: Model): Int = findMarker(input, 14) 
}
