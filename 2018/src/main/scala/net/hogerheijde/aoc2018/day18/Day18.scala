package net.hogerheijde.aoc2018.day18

import scala.collection.immutable
import scala.collection.mutable

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2018.Day2018
import scala.collection.immutable.IndexedSeq

object Day18 extends Day2018[Grid, Int, Int] {

  override def parse(input: String): Grid = {
    val m = input.trim.lines.zipWithIndex.foldLeft(immutable.Map.empty[Coordinate, Acre]) { case (mapping, (line, y)) =>
      line.zipWithIndex.foldLeft(mapping) { case (innerMapping, (char, x)) =>
        innerMapping.updated(Coordinate(x, y), Acre.forChar(char))
      }
    }
    Grid(m)
  }

  override def part1(input: Grid): Int = input.next(10).score

  override def part2(input: Grid): Int = input.next(300).score
}



trait Acre
object Acre {
  def forChar(c: Char): Acre = {
    c match {
      case '|' => Tree
      case '#' => Lumberyard
      case '.' => Open
    }
  }
}
case object Tree extends Acre { override val toString: String = "|" }
case object Lumberyard extends Acre  { override val toString: String = "#" }
case object Open extends Acre { override val toString: String = "." }


case class Grid(acres: Map[Coordinate, Acre]) {
  def score: Int = acres.count(_._2 == Tree) * acres.count(_._2 == Lumberyard)

  val maxX = acres.keySet.maxBy(_.x).x
  val maxY = acres.keySet.maxBy(_.y).y
  val subgrids: Map[Coordinate, (Acre, IndexedSeq[Acre])] = {
    Range.inclusive(0, maxY).foldLeft(Map.empty[Coordinate, (Acre, IndexedSeq[Acre])]) { case (mapping, y) =>
      Range.inclusive(0, maxX).foldLeft(mapping) { case (innerMapping, x) =>
        val coord = Coordinate(x = x, y = y)
        val subGrid: IndexedSeq[Acre] = IndexedSeq(
          acres.get(Coordinate(x = x - 1, y = y - 1)),
          acres.get(Coordinate(x = x, y = y - 1)),
          acres.get(Coordinate(x = x + 1, y = y - 1)),

          acres.get(Coordinate(x = x - 1, y = y)),
          acres.get(Coordinate(x = x + 1, y = y)),

          acres.get(Coordinate(x = x - 1, y = y + 1)),
          acres.get(Coordinate(x = x, y = y + 1)),
          acres.get(Coordinate(x = x + 1, y = y + 1)),
        ).flatten
        innerMapping.updated(coord, (acres(coord), subGrid))
      }
    }
  }

  def next(generations: Int): Grid = Stream.iterate(this)(_.next).drop(generations).head

  def next: Grid = {
    val newAcres = subgrids.foldLeft(Map.empty[Coordinate, Acre]) { case (mapping, (coordinate, (space, surrounding))) =>
      val newSpace = space match {
        case Open => if (surrounding.count(_ == Tree) >= 3) { Tree } else { Open }
        case Tree => if (surrounding.count(_ == Lumberyard) >= 3) { Lumberyard } else { Tree }
        case Lumberyard => if (surrounding.count(_ == Lumberyard) >= 1 && surrounding.count(_ == Tree) >= 1) { Lumberyard } else { Open }
      }
      mapping.updated(coordinate, newSpace)
    }

    val g = Grid(newAcres)

    println(g)
    println("***************************************")
    g
  }

  override def toString: String = {
    val b = new mutable.StringBuilder()
    Range.inclusive(0, maxY).foreach { y =>
      Range.inclusive(0, maxX).foreach { x =>
        b append acres(Coordinate(x, y))
      }
      b append "\n"
    }
    b.toString.trim
  }
}
