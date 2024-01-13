package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Day

object Day8 extends Day[Int, Int] {
  override type Model = Trees

  case class LineOfSight(
      coordinate: Coordinate,
      tree: Int,
      top: IndexedSeq[Int],
      bottom: IndexedSeq[Int],
      left: IndexedSeq[Int],
      right: IndexedSeq[Int],
  )

  case class Trees(
      rows: IndexedSeq[IndexedSeq[Int]],
      columns: IndexedSeq[IndexedSeq[Int]],
  ) {

    private def lineOfSight(c: Coordinate): LineOfSight = {
      val verticalSplit = columns(c.h).splitAt(c.v)
      val (top, bottom) = (verticalSplit._1, verticalSplit._2.tail) // remove item at index
      val horizontalSplit = rows(c.v).splitAt(c.h)
      val (left, right) = (horizontalSplit._1, horizontalSplit._2.tail) // remove item at index

      val tree = rows(c.v)(c.h)

      LineOfSight(
        coordinate = c,
        tree = tree,
        top = top.reverse,
        bottom = bottom,
        left = left.reverse,
        right = right,
      )
    }

    def isVisible(c: Coordinate): Boolean = {
      val los = lineOfSight(c)

      los.top.maxOption.forall(los.tree > _)  ||
        los.bottom.maxOption.forall(los.tree > _) ||
        los.left.maxOption.forall(los.tree > _) ||
        los.right.maxOption.forall(los.tree > _)
    }
    def scenicScore(c: Coordinate): Int =
      def scenicScorePart(s: Seq[Int], tree: Int): Int =
        s.indexWhereOption(_ >= tree).map(_ + 1).getOrElse(s.size)

      val los = lineOfSight(c)

      val x1 = scenicScorePart(los.left, los.tree) //.indexWhereOption(_ >= los.tree).map(_ + 1).getOrElse(los.left.size)
      val x2 = scenicScorePart(los.right, los.tree) //.indexWhereOption(_ >= los.tree).map(_ + 1).getOrElse(los.right.size)
      val x3 = scenicScorePart(los.top, los.tree) //.indexWhereOption(_ >= los.tree).map(_ + 1).getOrElse(los.top.size)
      val x4 = scenicScorePart(los.bottom, los.tree) //.indexWhereOption(_ >= los.tree).map(_ + 1).getOrElse(los.bottom.size)

      x1 * x2 * x3 * x4
  }

  override def parse(input: String): Model = {
    val rows: IndexedSeq[IndexedSeq[Int]] = input.linesIterator.toIndexedSeq.map(_.toIndexedSeq.map(char => char.asDigit))
    val cols: IndexedSeq[IndexedSeq[Int]] = rows.transpose

    Trees(rows, cols)
  }

  override def part1(input: Model): Int =
    Coordinate.range(Coordinate(0, 0), Coordinate(input.rows.size, input.columns.size)).count(input.isVisible(_))

  override def part2(input: Model): Int =
    Coordinate.range(Coordinate(0, 0), Coordinate(input.rows.size, input.columns.size)).map(input.scenicScore(_)).max


  extension (c: Char)
    def asDigit: Int = Integer.parseInt(c.toString, 10)

  extension (s: Seq[Int])
    def indexWhereOption(p: Int => Boolean): Option[Int] =
      s.indexWhere(p) match
        case i if i >= 0 => Some(i)
        case _ => None

}
