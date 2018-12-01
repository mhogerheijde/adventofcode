package net.hogerheijde.aoc2017.day12

import scala.annotation.tailrec

import net.hogerheijde.aoc2017.Day2017

object Day12 extends Day2017[Map[Int, Set[Int]], Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 12"

  override def parse: String => Map[Int, Set[Int]] = input => {
    input.lines.foldLeft(Map.empty[Int, Set[Int]]) { (result, currentLine) =>
      val parts = currentLine.trim.split(" <-> ")

      val left = parts(0).toInt
      val extraRights = parts(1).split(", ").map(_.toInt).toSet

      result.get(left) match {
        case Some(currentRights) => result.updated(left, currentRights ++ extraRights)
        case None => result.updated(left, extraRights)
      }
    }
  }
  override def part1(input: Map[Int, Set[Int]]): Int = resolve1(input, Set(), 0).size

  def resolve1(mapping: Map[Int, Set[Int]], seen: Set[Int], next: Int): Set[Int] = {
    if (seen.contains(next)) {
      seen
    } else {
      val nextSeen = seen + next
      mapping.get(next) match {
        case None => nextSeen
        case Some(others) =>
          others.foldLeft(nextSeen) { (currSeen, o) => resolve1(mapping, currSeen, o) }
      }
    }
  }


  override def part2(input: Map[Int, Set[Int]]): Int = resolve2(input, 0, 0)

  @tailrec
  def resolve2(input: Map[Int, Set[Int]], groupId: Int, count: Int): Int = {
    if (input.isEmpty) {
      0
    } else {
      val left = leftOver(input, groupId)
      left.keys.toIndexedSeq.sorted.headOption match {
        case None => count + 1
        case Some(nextGroupId) =>
          resolve2(left, nextGroupId, count + 1)
      }
    }
  }


  def leftOver(input: Map[Int, Set[Int]], groupId: Int): Map[Int, Set[Int]] = {
    val toRemove = resolve1(input, Set(), groupId)
    input.filter { case (key, _) => !toRemove.contains(key) }
  }





}
