package net.hogerheijde.aoc2020

import fastparse.NoWhitespace._
import fastparse._
import net.hogerheijde.aoc.util.Day
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc.util.Parser


sealed trait Square
case object Tree extends Square {
  override def toString: String = "#"
}
case object Open extends Square {
  override def toString: String = "."
}
case object Collision extends Square {
  override def toString: String = Console.RED + "X" + Console.RESET
}
case object Cleared extends Square {
  override def toString: String = Console.GREEN + "O" + Console.RESET
}

case class Row(row: IndexedSeq[Square]) {
  val size = row.size
  def get(index: Int) = row(index)
  override def toString: String = row.mkString("")
}

case class Slope(rows: IndexedSeq[(Row, Int)])

object Day3 extends Day[Int, Long] {

  type Model = Slope

  override def parse(input: String): Model = {
    val rows = input.linesIterator.map { line =>
      Row(Parser.parse(row(_))(line).getOrElse(
        throw new Exception(s"Could not parse $line")
      ))
    }.toIndexedSeq.zipWithIndex

    Slope(rows)
  }

  override def part1(input: Model): Int = {
    calculatePattern(3, 1, input)
  }

  override def part2(input: Model): Long = {
    val pt1 = calculatePattern(1, 1, input)
    val pt2 = calculatePattern(3, 1, input)
    val pt3 = calculatePattern(5, 1, input)
    val pt4 = calculatePattern(7, 1, input)
    val pt5 = calculatePattern(1, 2, input)

    pt1.toLong * pt2 * pt3 * pt4 * pt5
  }

  def calculatePattern(right: Int, down: Int, slope: Slope, doPrint: Boolean = false): Int = {
    if (doPrint) { println("\n---\n") }
    slope.rows.count { case (row, index) =>
      val squareIndex = (index / down) * right % row.size
      val isTree = index % down == 0 && row.get(squareIndex) == Tree
      output(doPrint, index % down == 0, squareIndex, isTree, row)
      isTree
    }
  }

  def output(doPrint: Boolean, currentRow: Boolean, squareIndex: Int, isTree: Boolean, row: Row): Unit = {
    if (doPrint) {
      if (currentRow) {
        println(Row(row.row.updated(
          squareIndex, if (isTree) { Collision } else { Cleared },
        )))
      } else {
        println(row)
      }
    }
  }

  def open[_ : P]: P[Open.type ] = P(".".!).map(q => Open)
  def tree[_ : P]: P[Tree.type] = P("#".!).map(q => Tree)
  def square[_: P]: P[Square] = P(open | tree)
  def row[_: P]: P[IndexedSeq[Square]] = P(square.rep).map(_.toIndexedSeq)

}
