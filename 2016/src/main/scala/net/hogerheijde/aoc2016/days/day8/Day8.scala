package net.hogerheijde.aoc2016.days.day8

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay

object Day8 extends RunnableDay {


  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day8.input")

    val screen = Day8.processPt1(input)
    println(s"Day 08 - pt1: ${screen.count} (expect 115)")
    val code = Alphabet.resolve(screen.toString)
    println(s"Day 08 - pt2: $code (expect EFEYKFRFIJ")

  }

//  def main(args: Array[String]): Unit = {
//    val input = Util.readFile("net/hogerheijde/aoc2016/days/day8.input")
//
//    val screen = new Screen(Grid(50, 6))
//    parse(input).foldLeft(screen) { case (screen, command) =>
//      val updated = command match {
//        case Rect(width, height) => screen.rect(width, height)
//        case RotateColumn(column, shift) => screen.rotateColumn(column, shift)
//        case RotateRow(row, shift) => screen.rotateRow(row, shift)
//      }
//      printf(s"\r${updated.toString}")
//      updated
//    }
//  }

  def processPt1(input: String): Screen = {
    val screen = new Screen(Grid(50, 6))
    val commands = parse(input)
    commands.foldLeft(screen) { case (screen, command) =>
      command match {
        case Rect(width, height) => screen.rect(width, height)
        case RotateColumn(column, shift) => screen.rotateColumn(column, shift)
        case RotateRow(row, shift) => screen.rotateRow(row, shift)
      }
    }
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
