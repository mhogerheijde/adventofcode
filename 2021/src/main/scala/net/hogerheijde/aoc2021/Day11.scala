package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.Grid.grid
import net.hogerheijde.aoc.text.AnsiHelpers._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser

object Day11 extends Day[Int, Int] {
  type Model = Grid[Byte]
  val Model = Grid

  implicit class DigitGridHelper(g: Model) {
    private def expand(coordinate: Coordinate): Set[Coordinate] = {
      Set(
        coordinate.transpose.leftUp,
        coordinate.transpose.up,
        coordinate.transpose.rightUp,
        coordinate.transpose.left,
        coordinate.transpose.right,
        coordinate.transpose.leftDown,
        coordinate.transpose.down,
        coordinate.transpose.rightDown,
      )
    }

    private def toFlashString: String = toFlashString(Seq())
    private def toFlashString(energizeList: Seq[Coordinate]): String = {
      val energized = energizeList.groupBy(identity).map { case (k, v) => (k, v.size) }
      val sorted = g.values.toSeq.sortBy { case (c, _) => (c.y, c.x) }
      val sb = new StringBuilder
      var lastY = sorted.headOption.map(_._1.y).getOrElse(0)
      sorted.foreach { case (c, v) =>
        if (c.y != lastY) {
          sb.append("\n")
          lastY = c.y
        }

        val toAppend = if (v >= 10) {
          "%X".format(v).bold
        } else if (v > 15) {
          "X"
        } else {
          energized.get(c) match {
            case None => "%X".format (v)
            case Some(count) => count.toString.green
          }
        }
        val toAppend2 = if (c == Coordinate(1, 5)) {
          toAppend.red
        } else {
          toAppend
        }

        sb.append(toAppend2)
      }
      sb.toString()
    }

    def step(amount: Int): (Model, Int) = {
      Range(0, amount).foldLeft((g ,0)) { case ((grid, total), _) =>
        val (newGrid, count) = grid.step
        (newGrid, count + total)
      }
    }

    def step: (Model, Int) = {
      def flash(grid: Model, currentFlashCount: Int) = {
        val flashed = grid.values.filter(_._2 >= 10).keys.toSet
        val energizeList = flashed.toSeq.flatMap(expand)
        val flashedGrid = flashed.foldLeft(grid) { case (newGrid, nextCoordinate) =>
          newGrid.copy(values = newGrid.values.updatedWith(nextCoordinate)(_.map(_ => -1)))
        }
        if (energizeList.nonEmpty) {
          energize(flashedGrid, energizeList, currentFlashCount + flashed.size)
        } else {
          val reset = Model(flashedGrid.values.view.mapValues(v => if (v < 0) { 0.toByte } else { v }).toMap)
          (reset, currentFlashCount)
        }
      }
      def energize(
          grid: Model,
          energize: Iterable[Coordinate],
          currentCount: Int,
      ): (Model, Int) = {
        val energizedGrid = energize.foldLeft(grid) { case (newGrid, nextCoordinate) =>
          // Updated with keeps empty coordinates emtpy if we return None for the update
          newGrid.copy(values =
            newGrid.values.updatedWith(nextCoordinate) { value =>
              value.map {
                case v if v >= 0 => (v + 1).toByte
                case v => v
              }
            }
          )
        }
        flash(energizedGrid, currentCount)
      }
      // Start the process
      energize(g, g.values.keys, 0)
    }
  }

  override def parse(input: String): Model = Parser.parse(grid(_))(input).get
  override def part1(input: Model): Int = input.step(100)._2
  override def part2(input: Model): Int = {
    val x = LazyList
      .iterate((input, 0))(_._1.step)
      .zipWithIndex
      .find { case ((_, flashCount), step) => flashCount == 100 }
    x.get._2
  }
}
