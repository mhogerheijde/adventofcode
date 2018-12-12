package net.hogerheijde.aoc.util

import scala.collection.immutable.IndexedSeq

import fastparse.P
import fastparse.Parsed
import net.hogerheijde.aoc.common.parser.Common.intSeq

object Parser {


  def standardIntSplit(input: String): IndexedSeq[Int] = parse(intSeq(_))(input).get
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
