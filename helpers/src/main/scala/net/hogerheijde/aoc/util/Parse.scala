package net.hogerheijde.aoc.util

import scala.collection.immutable.IndexedSeq

object Parse {

  def standardLineSplit(in: String): IndexedSeq[String] = in.trim.split("\n").toIndexedSeq.map(_.trim)

}
