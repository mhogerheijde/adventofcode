package net.hogerheijde.aoc2018.day5


object Reactor {

  def reduceFiltered(input: String, filter: Char): String = {
    input.filterNot(p => p.toLower == filter.toLower).foldLeft("") { (result, nextChar) =>
      if (Reactor.reactsWith(nextChar, result.takeRight(1))) {
        result.init
      } else {
        result + nextChar
      }
    }
  }

  def reduce(input: String): String = {
    input.foldLeft("") { (result, nextChar) =>
      if (Reactor.reactsWith(nextChar, result.takeRight(1))) {
        result.init
      } else {
        result + nextChar
      }
    }
  }

  def reactsWith(in: Char, other: String): Boolean = {
    isSameType(in, other) && haveDifferentPolarity(in, other)
  }

  protected[day5] def isSameType(a: Char, b: String): Boolean = b.length == 1 && isSameType(a, b.head)
  protected[day5] def isSameType(a: Char, b: Char): Boolean = a.toLower == b.toLower
  protected[day5] def haveDifferentPolarity(a: Char, b: String): Boolean = b.length == 1 && ((a.isUpper && b.head.isLower) || (a.isLower && b.head.isUpper))
}