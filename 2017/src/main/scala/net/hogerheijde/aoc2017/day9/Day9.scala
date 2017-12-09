package net.hogerheijde.aoc2017.day9

import net.hogerheijde.aoc2017.Day

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq



object Day9 extends Day[Group, Int, Int] {
  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 9"
  override def parse: String => Group = i => {
    consumeThing(i.split("").toList.map(_.head), 1)._1.get.asInstanceOf[Group]
  }

  override def part1(input: Group): Int = scoreOf(input)
  override def part2(input: Group): Int = countOfGarbage(input)

  def scoreOf(input: Thing): Int = {
    input match {
      case Group(level, things) =>
        level + things.map(scoreOf).sum
      case Garbage(_, _) => 0
    }
  }

  def countOfGarbage(input: Thing): Int = {
    input match {
      case Group(_, things) =>
        things.map(countOfGarbage).sum
      case Garbage(_, count) => count
    }
  }

  @tailrec
  def consumeThing(remainder: List[Char], level: Int = 0 ): (Option[Thing], List[Char]) = {
    remainder match {
      case head::tail =>
        head match {
          case '<' =>
            val (tailWithoutGarbage, garbageRemoved) = consumeGarbage(tail)
            (Some(Garbage(level, garbageRemoved)), tailWithoutGarbage)
          case '{' =>
            val (things: IndexedSeq[Thing], newTail: List[Char]) = consumeThings(tail, level + 1)
            (Some(Group(level, things)), newTail)
          case '}' =>
            (None, tail)
          case _ =>
            consumeThing(tail, level)
        }
      case Nil => (None, Nil)
    }
  }

  def consumeThings(things: List[Char], level: Int): (IndexedSeq[Thing], List[Char]) = {
    val (thing_?, left) = consumeThing(things, level)
    thing_? match {
      case None => (IndexedSeq(), left)
      case Some(thing) => {
        val (moreThings, remainder) = consumeThings(left, level)
        (thing +: moreThings, remainder)
      }
    }

  }

  @tailrec
  def consumeGarbage(reimainder: List[Char], count: Int = 0): (List[Char], Int) = {
    reimainder match {
      case head :: tail =>
        head match {
          case '!' =>
            consumeGarbage(tail.tail, count)
          case '>' =>
            (tail, count)
          case _ =>
            consumeGarbage(tail, count + 1)
        }
      case List() => (Nil, 0)
    }
  }
}
