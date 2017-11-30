package net.hogerheijde.aoc2016.days.day13

import scala.annotation.tailrec

class Grid (val designer: Int = 1358) {

  def generateTile(x: Int, y: Int): Tile = {

    val tileNumber = (x*x + 3*x + 2*x*y + y + y*y) + designer
    val tileInBin = Grid.toBin(tileNumber)
    if (tileInBin.count(_ == '1') % 2 == 0) {
      Hall(x, y)
    } else {
      Wall(x, y)
    }
  }

  def output(width: Int, height: Int): String = {

    val header = "  " + Range(0, width).map(_ % 10).mkString

    val rows: IndexedSeq[String] = for { y <- Range(0, height) } yield {
      val row = for { x <- Range(0, width) } yield { generateTile(x, y).output }
      s"${y % 10} " + row.mkString
    }

    header + "\n" + rows.mkString("\n")

  }
}

object Grid {

  @tailrec
  def toBin(i: Int, current: String = ""): String = {
    if (i == 0) {
      if (current.isEmpty) { "0" } else { current }
    } else {
      val divide = i / 2

      if (i % 2 == 0) {
        toBin(divide, "0" + current)
      } else {
        toBin(divide, "1" + current)
      }
    }
  }

}

trait Tile {
  val x: Int
  val y: Int
  def output: String
}
case class Wall(x: Int, y: Int) extends Tile { override def output = "#" }
case class Hall(x: Int, y: Int) extends Tile { override def output = "." }
