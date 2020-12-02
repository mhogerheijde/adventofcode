package net.hogerheijde.aoc.util

object Implicits {

  implicit class GroupByTupleHelper[A, B](iterable: Iterable[(A,B)]) {
    def groupByFirstUnique: Map[A, B] = iterable.groupBy(_._1).view.mapValues(_.head._2).toMap
  }

  implicit class GroupByHelper[T](iterable: Iterable[T]) {
    def groupByUnique[K](f : T => K): Map[K, T] = iterable.groupBy(f).view.mapValues(_.head).toMap
  }

  implicit class TextHelpers(s: String) {

    val reset = "\u001b[0m"

    def black: String = s"\u001b[30m$s$reset"
    def red: String = s"\u001b[31m$s$reset"
    def green: String = s"\u001b[32m$s$reset"
    def yellow: String = s"\u001b[33m$s$reset"
    def blue: String = s"\u001b[34m$s$reset"
    def magenta: String = s"\u001b[35m$s$reset"
    def cyan: String = s"\u001b[36m$s$reset"
    def white: String = s"\u001b[37m$s$reset"

  }

  implicit class BooleanHelper(a: Boolean) {
    def xor(b: Boolean): Boolean = (a || b) && !(a && b)
  }

}
