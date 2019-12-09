package net.hogerheijde.aoc2019

import net.hogerheijde.aoc.util.Day

trait Day2019[Model, Result1, Result2] extends Day[Model, Result1, Result2] {

  def name: String = { "Day " + getClass.getSimpleName.drop(3).dropRight(1).mkString("") }

  val year = "aoc2019"
}
