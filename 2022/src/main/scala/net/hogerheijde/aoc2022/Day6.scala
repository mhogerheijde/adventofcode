package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.util.Day

object Day6 extends Day[Int, Int] {
  override type Model = String
  
  override def parse(input: String): Model = input

  def findMarker(input: Model, windowSize: Int): Int = {
    input.toSeq.sliding(windowSize).indexWhere(_.toSet.size == windowSize) + windowSize
  } 
  
  override def part1(input: Model): Int = findMarker(input, 4)
  override def part2(input: Model): Int = findMarker(input, 14) 
}
