package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Day

object Day8 extends Day[Int, Int] {


  override type Model = Trees

  case class Trees(
      rows: IndexedSeq[IndexedSeq[Int]],
      columns: IndexedSeq[IndexedSeq[Int]],
  ) {
    def isVisible(c: Coordinate): Boolean = {
      val verticalSplit = columns(c.h).splitAt(c.v)
      val (top, bottom) = (verticalSplit._1, verticalSplit._2.tail) // remove item at index
      val horizontalSplit = rows(c.v).splitAt(c.h)
      val (left, right) = (horizontalSplit._1, horizontalSplit._2.tail) // remove item at index

      val tree = rows(c.v)(c.h)

      top.maxOption.forall(tree > _)  ||
        bottom.maxOption.forall(tree > _) ||
        left.maxOption.forall(tree > _) ||
        right.maxOption.forall(tree > _)
    }
  }

  override def parse(input: String): Model = {
    val rows: IndexedSeq[IndexedSeq[Int]] = input.linesIterator.toIndexedSeq.map(_.toIndexedSeq.map(char => char.asDigit))
    val cols: IndexedSeq[IndexedSeq[Int]] = rows.transpose.toIndexedSeq

    Trees(rows, cols)
  }

  override def part1(input: Model): Int = Coordinate.range(Coordinate(0, 0), Coordinate(input.rows.size, input.columns.size)).count(input.isVisible(_))

  override def part2(input: Model): Int = ???


  extension (c: Char)
    def asDigit: Int = Integer.parseInt(c.toString, 10)

}
