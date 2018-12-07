package net.hogerheijde.aoc2018.day2

import net.hogerheijde.aoc2018.Day2018
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.util.Parser

object Day2 extends Day2018[IndexedSeq[String], Int, String]{

  override def name: String = "Day 2"
  override def parse(in: String): IndexedSeq[String] = Parser.standardLineSplit(in)
  override def part1(input: IndexedSeq[String]): Int = {
    countDoubles(input) * countTriplets(input)

  }
  override def part2(input: IndexedSeq[String]): String = {
    input.combinations(2).collectFirst {
      case candidates if candidate(candidates(0), candidates(1)) =>
        val pairs = candidates(0).zip(candidates(1))
        pairs.filterNot(p => p._1 != p._2).map(_._1).mkString("")
    } match {
      case None => "Could not find a candidate ID"
      case Some(id) => id
    }
  }

  protected[day2] def candidate(s1: String, s2: String): Boolean = {
    val pairs = s1.zip(s2)
    pairs.count(p => p._1 != p._2) == 1
  }



  protected[day2] def histogram(s: String): Map[Char, Int] = {
    s.groupBy(c => c).mapValues(_.length)
  }


  protected[day2] def countDoubles(list: IndexedSeq[String]): Int = {
    list.foldLeft(0) { case (count, nextItem) =>
      val hasDoubles = histogram(nextItem).filter(_._2 == 2).nonEmpty
      if(hasDoubles) {
        count + 1
      } else {
        count
      }
    }
  }
  protected[day2] def countTriplets(list: IndexedSeq[String]): Int = {
    list.foldLeft(0) { case (count, nextItem) =>
      val hasTriplets = histogram(nextItem).filter(_._2 == 3).nonEmpty
      if(hasTriplets) {
        count + 1
      } else {
        count
      }
    }
  }
}
