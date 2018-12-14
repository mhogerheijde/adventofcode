package net.hogerheijde.aoc.util

object Implicits {

  implicit class GroupByTupleHelper[A, B](iterable: Iterable[(A,B)]) {
    def groupByFirstUnique: Map[A, B] = iterable.groupBy(_._1).mapValues(_.head._2)
  }

  implicit class GroupByHelper[T](iterable: Iterable[T]) {
    def groupByUnique[K](f : T => K): Map[K, T] = iterable.groupBy(f).mapValues(_.head)
  }
}
