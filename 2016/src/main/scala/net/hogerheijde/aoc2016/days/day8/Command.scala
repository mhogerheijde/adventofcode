package net.hogerheijde.aoc2016.days.day8

trait Command
case class Rect(width: Int, height: Int) extends Command
case class RotateColumn(column: Int, shift: Int) extends Command
case class RotateRow(row: Int, shift: Int) extends Command
