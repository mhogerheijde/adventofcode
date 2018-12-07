package net.hogerheijde.aoc.util

import scala.collection.immutable.IndexedSeq

import fastparse.P
import fastparse.Parsed

object Parser {

  def standardLineSplit(in: String): IndexedSeq[String] = in.trim.split("\n").toIndexedSeq.map(_.trim)

  def parse[T](p: P[_] => P[T])(input: String): Option[T] = {
    fastparse.parse(input, p) match {
      case Parsed.Success(result, _) => Some(result)
      case Parsed.Failure(_, _, e) =>
        val t = e.trace()
        println(s"Tried to parse the following input, but failed:\n${input}\nReason: $t")
        None
    }
  }
}
