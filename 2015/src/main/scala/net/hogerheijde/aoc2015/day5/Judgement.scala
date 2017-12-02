package net.hogerheijde.aoc2015.day5

trait Judgement
object Judgement {
  def apply(isNice: Boolean): Judgement = {
    if (isNice) { Nice } else { Naughty }
  }
}
case object Naughty extends Judgement
case object Nice extends Judgement