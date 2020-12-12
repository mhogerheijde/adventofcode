package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Day

object Day10 extends Day[Long, Long]{
  type Model = Seq[Long]

  override def parse(input: String): Day10.Model = {
    val read = input.linesIterator.map(_.toLong).toSeq
    (0L +: read :+ (read.max + 3))
  }

  override def part1(input: Day10.Model): Long = {
    val differences = input
      .sorted
      .sliding(2)
      .map { case head +: tail => tail.foldLeft(head) { (acc, next) => next - acc }}
      .toSeq

    println(differences)
    differences.count(_ == 1) * differences.count(_ == 3)
  }

  val pathCountByConsequtive = Map(
    0 -> 1L,
    1 -> 1L,
    2 -> 2L,
    3 -> 4L,
    4 -> 7L,
  )

  override def part2(input: Day10.Model): Long = input
    .sorted
    .sliding(2)
    .map { case head +: tail => tail.foldLeft(head) { (acc, next) => next - acc }}
    .foldLeft(Seq(0)) {
      case (acc, 1) => acc.init :+ (acc.last + 1)
      case (acc, _) => acc :+ 0
    }
    .map(pathCountByConsequtive)
    .product
}
