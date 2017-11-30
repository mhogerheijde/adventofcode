package net.hogerheijde.aoc2016.days.day3

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay
import net.hogerheijde.aoc2016.days.day3.model.Triangle

import scala.util.Try

object Day3 extends RunnableDay {

  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day3.input")
    val triangles1 = Day3.processAsRows(input)
    println(s"Day 03 - pt1: ${triangles1.length} (expect 1050)")
    val triangles2 = Day3.processAsColumns(input)
    println(s"Day 03 - pt2: ${triangles2.length} (expect 1921)")
  }


  def processAsRows(input: String): IndexedSeq[Triangle] = {
    val sidess = parseRows(input)
    sidess.flatMap(buildTriangle)
  }

  def processAsColumns(input: String): IndexedSeq[Triangle] = {
    val sidess = parseColumns(input)
    sidess.flatMap(buildTriangle)
  }

  def parseRows(input: String): IndexedSeq[IndexedSeq[Int]] = {
    input.trim().split("\n").map(parseLine)
  }

  def parseLine(line: String): IndexedSeq[Int] = {
    line.split(" +").flatMap(side => Try(side.toInt).toOption).toIndexedSeq
  }

  def parseColumns(input: String): IndexedSeq[IndexedSeq[Int]] = {

    val empty: IndexedSeq[IndexedSeq[Int]] = IndexedSeq()
    val emptyPartial: (Option[(Int, Int, Int)], Option[(Int, Int, Int)]) = (None, None)

    val (triangleSides, _) = input.trim().split("\n").foldLeft( (empty, emptyPartial) ) { case ((accSides, partial), line) =>

      val sides = parseLine(line)
      val (x_, y_, z_) = (sides(0), sides(1), sides(2))
      partial match {
        case (None, None) => (accSides, (Some( (x_, y_, z_) ), None))
        case (Some(parts), None) => (accSides, (Some(parts), Some( (x_, y_, z_) )))
        case (Some( (x1, y1, z1) ), Some( (x2, y2, z2) )) =>

          val newTriangles = IndexedSeq(
            IndexedSeq(x1, x2, x_),
            IndexedSeq(y1, y2, y_),
            IndexedSeq(z1, z2, z_)
          )
          (accSides ++ newTriangles, (None, None))
        case _ => (accSides, (None, None))
      }
    }

    // TODO: Check if there are no partials left?
    triangleSides
  }



  def buildTriangle(sides: IndexedSeq[Int]): Option[Triangle] = {
    sides.sorted match {
      case IndexedSeq(one, two, three) =>
        if (one + two > three) {
          Some(Triangle(one, two, three))
        } else {
          None
        }
      case _ => None
    }

  }

}
