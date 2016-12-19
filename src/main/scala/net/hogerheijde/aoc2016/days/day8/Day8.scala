package net.hogerheijde.aoc2016.days.day8

import net.hogerheijde.aoc2016.Util

object Day8 {

  def main(args: Array[String]): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day8.input")

    val screen = new Screen(Grid(50, 6))
    parse(input).foldLeft(screen) { case (screen, command) =>
      val updated = command match {
        case Rect(width, height) => screen.rect(width, height)
        case RotateColumn(column, shift) => screen.rotateColumn(column, shift)
        case RotateRow(row, shift) => screen.rotateRow(row, shift)
      }
      printf(s"\r${updated.toString}")
      updated
    }
  }

  def processPt1(input: String): Int = {
    val screen = new Screen(Grid(50, 6))
    val commands = parse(input)

    val result = commands.foldLeft(screen) { case (screen, command) =>
      val s = command match {
        case Rect(width, height) => screen.rect(width, height)
        case RotateColumn(column, shift) => screen.rotateColumn(column, shift)
        case RotateRow(row, shift) => screen.rotateRow(row, shift)
      }
//      println(s)
//      println("------------------------------------------------------------")

      s
    }
    println(result)
    result.count
  }

  def parse(input: String): IndexedSeq[Command] = {
    input.split("\n").flatMap(parseCommand)
  }

  val rectPattern = "rect ([0-9]+)x([0-9]+)".r
  val columnPattern = "rotate column x=([0-9]+) by ([0-9]+)".r
  val rowPattern = "rotate row y=([0-9]+) by ([0-9]+)".r
  def parseCommand(input: String): Option[Command] = {
    input match {
      case rectPattern(width, height) => Some(Rect(width = width.toInt, height = height.toInt))
      case columnPattern(column, shift) => Some(RotateColumn(column.toInt, shift.toInt))
      case rowPattern(row, shift) => Some(RotateRow(row.toInt, shift.toInt))
      case _ => None
    }
  }

}
