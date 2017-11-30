package net.hogerheijde.aoc2016.days.day8

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Map

class Screen(grid: Grid) {

  lazy val pixelsByRow: IndexedSeq[IndexedSeq[(Coordinate, Pixel)]] = {
    Range(0, grid.height) map { vertical =>
      Range(0, grid.width) map { horizontal =>
        val coord = Coordinate(horizontal = horizontal, vertical = vertical)
        (coord, grid(coord))
      }
    }
  }

  lazy val pixelsByColumn: IndexedSeq[IndexedSeq[(Coordinate, Pixel)]] = {
    Range(0, grid.width) map { horizontal =>
      Range(0, grid.height) map { vertical =>
        val coord = Coordinate(horizontal = horizontal, vertical = vertical)
        (coord, grid(coord))
      }
    }
  }

  def count: Int = grid.count

  def rect(width: Int, height: Int): Screen = {
    val coordinatesToTurnOn = for {
      vertical <- Range(0, height)
      horizontal <- Range(0, width)
    } yield {
      Coordinate(horizontal = horizontal, vertical = vertical)
    }

    new Screen(coordinatesToTurnOn.foldLeft(grid) { case (updatedGrid, nextCoordinate) =>
      updatedGrid.update(nextCoordinate, On)
    })
  }

  def rotateColumn(column: Int, amount: Int): Screen = {
    new Screen(pixelsByColumn(column).foldLeft(grid) { case (updatedGrid, (coordinate, pixel)) =>
      val newCoordinate = coordinate.copy(vertical = (coordinate.vertical + amount) % grid.height )
      updatedGrid.update(newCoordinate, pixel)
    })
  }

  def rotateRow(row: Int, amount: Int): Screen = {
    new Screen(pixelsByRow(row).foldLeft(grid) { case (updatedGrid, (coordinate, pixel)) =>
      val newCoordinate = coordinate.copy(horizontal = (coordinate.horizontal+ amount) % grid.width )
      updatedGrid.update(newCoordinate, pixel)
    })
  }


  override def toString = {
    (pixelsByRow map { row =>
      row.map(_._2.toString).mkString
    }).mkString("\n")
  }
}

class Grid(pixels: Map[Coordinate, Pixel]) {
  lazy val width: Int = pixels.keys.toIndexedSeq.sortBy(_.horizontal).last.horizontal + 1
  lazy val height: Int = pixels.keys.toIndexedSeq.sortBy(_.vertical).last.vertical + 1
  lazy val count: Int = pixels.values.count(p => p == On)
  def update(coordinate: Coordinate, pixel: Pixel): Grid = Grid(pixels.updated(coordinate, pixel))
  def apply(coordinate: Coordinate): Pixel = pixels.withDefaultValue(Off)(coordinate)
}
object Grid {
  def apply(pixels: Map[Coordinate, Pixel]): Grid = new Grid(pixels)
  def apply(width: Int, height: Int): Grid = {
    val pixels = (for {
      vertical <- Range(0, height)
      horizontal <- Range(0, width)
    } yield {
      (Coordinate(horizontal = horizontal, vertical = vertical), Off)
    }).toMap
    new Grid(pixels)
  }
}

case class Coordinate(horizontal: Int, vertical: Int)

trait Pixel
case object On extends Pixel {
  override def toString = "#"
}
case object Off extends Pixel {
  override def toString = "."
}
