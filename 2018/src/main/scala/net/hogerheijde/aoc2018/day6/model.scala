package net.hogerheijde.aoc2018.day6

import java.lang.Math.abs

import net.hogerheijde.aoc.common.model.Coordinate


case class NamedCoordinate(id: Char, coordinate: Coordinate) {
  def distance(other: Coordinate): Int = abs(coordinate.x - other.x) + abs(coordinate.y - other.y)
}
object NamedCoordinate {
  def apply(id: Char, xy: (Int, Int)): NamedCoordinate = new NamedCoordinate(id, Coordinate(xy._1, xy._2))
}

trait GridLike {
  def coordinates: IndexedSeq[NamedCoordinate]
  def originalPoints: IndexedSeq[NamedCoordinate]

  def left: Int
  def right: Int
  def top: Int
  def bottom: Int

  lazy val coordinatesByCoordinate = coordinates.map(_.coordinate).zip(coordinates).toMap
  lazy val originalPointsByCoordinate = originalPoints.map(_.coordinate).zip(originalPoints).toMap

  override def toString: String = {
    Range.inclusive(top - 1, bottom + 1).map { line =>
      // For every line
      Range.inclusive(left -1, right + 1).foldLeft("") { case (resultString, pos) =>
        val pointToDraw = Coordinate(pos, line)
        val maybeCoordinate = originalPointsByCoordinate
          .get(pointToDraw).map(Some(_))
          .getOrElse(coordinatesByCoordinate.get(pointToDraw))
        val charToPrint = maybeCoordinate.map(_.id).getOrElse('.')
        resultString + charToPrint
      }

    }.mkString("\n")
  }
}

case class Grid(coordinates: IndexedSeq[NamedCoordinate]) extends GridLike {

  val originalPoints = coordinates.filter(_.id.isUpper)

  val left: Int   = originalPoints.minBy(c => (c.coordinate.x, c.coordinate.y)).coordinate.x
  val right: Int  = originalPoints.maxBy(c => (c.coordinate.x, c.coordinate.y)).coordinate.x
  val top: Int    = originalPoints.minBy(c => (c.coordinate.y, c.coordinate.x)).coordinate.y
  val bottom: Int = originalPoints.maxBy(c => (c.coordinate.y, c.coordinate.x)).coordinate.y

  def withAreasResolved: AreaGrid = {
    val newCoordinates = Range.inclusive(top - 1, bottom + 1).flatMap { line =>
      // For every line
      Range.inclusive(left - 1, right + 1).flatMap{ pos =>
          val currentPoint = Coordinate(pos, line)

          originalPointsByCoordinate.get(currentPoint) match {
            case None =>
              val distMap = originalPoints.map(original => (original.distance(currentPoint), original)).groupBy(_._1)
              val minDist = distMap.keys.min

              // must exist, so shouldn't throw
              distMap(minDist) match {
                case IndexedSeq((_, single)) => Some(NamedCoordinate(single.id.toLower, currentPoint))
                case _ => None
              }

            case original => original
          }
      }
    }
    AreaGrid(newCoordinates, originalPoints, left, right, top, bottom)
  }

  def toRegionGrid(limit: Int): RegionGrid = {
    val q = Range.inclusive(top - 1, bottom + 1).flatMap { line =>
      // For every line
      Range.inclusive(left - 1, right + 1).flatMap { pos =>
        val currentPoint = Coordinate(pos, line)
        val distances = originalPoints.map(_.distance(currentPoint))
        val totalDists = distances.sum
        if (totalDists < limit) {
          Some(NamedCoordinate('#', currentPoint))
        } else {
          None
        }
      }
    }
    RegionGrid(q, originalPoints, left, right, top, bottom)
  }





}

case class AreaGrid(
    coordinates: IndexedSeq[NamedCoordinate],
    originalPoints: IndexedSeq[NamedCoordinate],
    left: Int, right: Int, top: Int, bottom: Int) extends GridLike {
  def areaFor(id: Char): Option[Int] = {
    if (infinteAreas.contains(id.toUpper)) {
      None
    } else {
      Some(coordinates.count(_.id.toUpper == id.toUpper))
    }
  }

  def maxArea: Int = {
    originalPoints.map(_.id).flatMap(areaFor).max
  }

  def infinteAreas: Set[Char] = {
    coordinates.filter(c =>
      c.coordinate.x == left ||
        c.coordinate.y == top ||
        c.coordinate.x == right ||
        c.coordinate.y == bottom
    ).map(_.id.toUpper).toSet
  }
}

case class RegionGrid(coordinates: IndexedSeq[NamedCoordinate],
    originalPoints: IndexedSeq[NamedCoordinate],
    left: Int, right: Int, top: Int, bottom: Int) extends GridLike {
  def regionSize: Int = coordinates.count(_.id=='#')
}