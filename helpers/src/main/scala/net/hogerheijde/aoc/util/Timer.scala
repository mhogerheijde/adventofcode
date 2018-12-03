package net.hogerheijde.aoc.util

import scala.concurrent.duration.Duration

object Timer {

  case class TimedResult[T](value: T, duration: Duration) {
    override def toString: String = s"$value, took ${duration}"
  }
  def apply[T](f: => T): TimedResult[T] = {
    val start = System.nanoTime
    val t = f
    val end = System.nanoTime
    TimedResult(t, Duration.fromNanos(end - start))
  }

}
