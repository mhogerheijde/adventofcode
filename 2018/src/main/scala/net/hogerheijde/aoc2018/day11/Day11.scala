package net.hogerheijde.aoc2018.day11

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.util.Timer
import net.hogerheijde.aoc.util.Timer.TimedResult
import net.hogerheijde.aoc2018.Day2018

object Day11 extends Day2018[Grid, String, String] {
  override def name: String = "Day 11"

  override def parse(input: String): Grid = Grid.build(input.toInt, 300)

  override def part1(input: Grid): String = {
    val grids = input.allSubgridPowerssOfSize(3)
    val bestGrid = grids.maxBy(_._2)
    bestGrid.toString()
  }

  override def part2(input: Grid): String = {


//    val TimedResult(_, time1) = Timer { input.subgrid(Coordinate(1, 1), 200) }
//    val TimedResult(_, time2) = Timer { input.subgridPower(Coordinate(1, 1), 200) }


    val grids = Range.inclusive(1, 300).par.map { size =>
      val TimedResult(maxGrid, time) = Timer {
        val grids = input.allSubgridPowerssOfSize(size)
        val maxGrid = grids.maxBy(_._2)
        (maxGrid._1, size, maxGrid._2)
      }
      println(s"Finding max for size $size took $time")
      maxGrid
    }

    val maxGrid = grids.maxBy(_._3)
    maxGrid.toString()
  }
}



case class Grid(cells: Map[Coordinate, Int]) {

  def power: Int = cells.values.sum

  def allSubgridsOfSize(size: Int): Map[Coordinate, Grid] = {
    Range.inclusive(1, 300 - size).foldLeft(Map.empty[Coordinate, Grid]) { case (totalGrids, y) =>
      Range.inclusive(1, 300 - size).foldLeft(totalGrids) { case (grids, x) =>
        val coord = Coordinate(x, y)
        grids.updated(coord, this.subgrid(coord, size))
      }
    }
  }

  def allSubgridPowerssOfSize(size: Int): Map[Coordinate, Int] = {
    Range.inclusive(1, 300 - size).foldLeft(Map.empty[Coordinate, Int]) { case (totalGrids, y) =>
      Range.inclusive(1, 300 - size).foldLeft(totalGrids) { case (grids, x) =>
        val coord = Coordinate(x, y)
        grids.updated(coord, this.subgridPower(coord, size))
      }
    }
  }

  def subgridPower(corner: Coordinate, size: Int): Int = {
    val powers = Range(corner.y, corner.y + size).flatMap { y: Int =>
      Range(corner.x, corner.x + size).map { x =>
        cells(Coordinate(x, y))
      }
    }
    powers.sum
  }

  def subgrid(corner: Coordinate, size: Int): Grid = {
    val coordinates = Range(corner.y, corner.y + size).foldLeft(Map.empty[Coordinate, Int]) { case (totalCells, y) =>
      Range(corner.x, corner.x + size).foldLeft(totalCells) { case (newCells, x) =>
        val coord = Coordinate(x, y)
        newCells.updated(coord, cells(coord))
      }
    }
    Grid(coordinates)
  }

  override def toString: String = {

    val maxX = cells.keySet.map(_.x).max
    val minX = cells.keySet.map(_.x).min
    val maxY = cells.keySet.map(_.y).max
    val minY = cells.keySet.map(_.y).min

    val b = new StringBuilder

    Range.inclusive(minY, maxY).foreach { y => // lines
      Range.inclusive(minX, maxX).foreach { x => // columns
        b append cells(Coordinate(x, y)).toString.padTo(5, " ").mkString("")
        b append " "
      }
      b append "\n"
    }
    b.toString
  }


}
object Grid {

  def build(seed: Int, size: Int): Grid = {
    val cells = Range.inclusive(1, size).foldLeft(Map.empty[Coordinate, Int]) { case (outerCells, y) =>
      Range.inclusive(1, size).foldLeft(outerCells) { case (cells, x) =>
        cells.updated(Coordinate(x, y), calculateValue(x, y, seed))
      }
    }

    Grid(cells)
  }

  def calculateValue(x: Int, y: Int, seed: Int): Int = {
    val rackId = x + 10
    val start = rackId * y
    val withSerial = start + seed
    val multiplied = withSerial * rackId
    val hundreds = ((multiplied /100)%10)
    hundreds - 5
  }


}