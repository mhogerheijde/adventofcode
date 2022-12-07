package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc2022.Day3.Item.A
import net.hogerheijde.aoc2022.Day3.Item.a
import net.hogerheijde.aoc2022.Day3.Item.allowed

object Day3 extends Day[Int, Int] {

  override type Model = Seq[Rucksack]

  type Item = Char
  case class Rucksack(items: Seq[Item]){
    require(items.forall(allowed(_)))
    val compartment1: Set[Item] = items.take(items.length / 2).toSet
    val compartment2: Set[Item] = items.drop(items.length / 2).toSet

    override def toString: String =
      s"""Rucksack<
         |  c1: ${compartment1.mkString(", ")}
         |  c2: ${compartment2.mkString(", ")}
         |>""".stripMargin
  }
  object Rucksack {
    def apply(in: String): Rucksack = Rucksack(in.toSeq)
  }

  implicit class ItemHelper(in: Item) {
    def priority: Int = if (in.isUpper) { in.toInt - A + 27 } else { in.toInt - a + 1 }
  }

  object Item {
    val a = 'a'.toInt
    val A = 'A'.toInt
    val allowed: Set[Char] =
      Range(a, a + 26).map(_.toChar).toSet ++
        Range(A, A + 26).map(_.toChar).toSet
  }


  override def parse(input: String): Model = input.linesIterator.map(Rucksack(_)).toSeq

  override def part1(input: Model): Int = input
    .map { rucksack => rucksack.compartment1.intersect(rucksack.compartment2) }
    .map { _.map(_.priority).sum }
    .sum

  override def part2(input: Model): Int = input.sliding(3, 3)
    .map { group =>
      group.foldLeft(allowed) { case (acc, nextSack) => acc.intersect(nextSack.items.toSet) }
    }
    .map { _.map(_.priority).sum }
    .sum
}
