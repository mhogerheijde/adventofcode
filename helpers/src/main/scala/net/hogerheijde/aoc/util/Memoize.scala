package net.hogerheijde.aoc.util

import scala.collection.mutable

/**
  * https://stackoverflow.com/questions/16257378/is-there-a-generic-way-to-memoize-in-scala/36960228#36960228
  */
def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  self => override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}