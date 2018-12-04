package net.hogerheijde.aoc2018.day4

import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import fastparse.NoWhitespace._
import fastparse._


object Model {

  type NightshiftMapping = Map[(Int, LocalDateTime), Nightshift]

  case class Interval(start: LocalDateTime, end: LocalDateTime) {
    def sleepMinutes: Long = Duration.between(start, end).toMinutes
    def contains(minute: Int): Boolean = start.getMinute <= minute && minute < end.getMinute
  }
  object Interval {
    def apply(start: String, end: String): Interval = new Interval(
      LocalDateTime.parse(start.replace(" ", "T")),
      LocalDateTime.parse(end.replace(" ", "T")))
  }
  case class Nightshift(guardId: Int, start: LocalDateTime, sleepyTimes: Seq[Interval]) {
    def sleepMinutes: Long = sleepyTimes.map(_.sleepMinutes).sum
  }
  object Nightshift {
    def apply(
        guardId: Int,
        start: String,
        sleepyTimes: Seq[Interval]): Nightshift = new Nightshift(
      guardId,
      LocalDateTime.parse(start.replace(" ", "T")),
      sleepyTimes)
  }

  sealed trait Event
  object Event{
    def parse(in: String): Option[(LocalDateTime, Event)] = {
      fastparse.parse(in, Parsers.line(_)) match {
        case Parsed.Success(event, _) => Some(event)
        case Parsed.Failure(_, _, e) =>
          val t = e.trace()
          println(t)
          None
      }
    }
  }
  case object FallsAsleep extends Event
  case object WakesUp extends Event
  case class ShiftStarts(guard: Int) extends Event

  object Parsers {

    def line[_: P]: P[(LocalDateTime, Event)] = P(dateTime ~ " " ~ event)

    private def event[_: P]: P[Event] = P(shiftStarts | fallsAsleep | wakesUp)

    private def shiftStarts[_: P]: P[Event] = P("falls asleep").map(_ => FallsAsleep)
    private def fallsAsleep[_: P]: P[Event] = P("wakes up").map(_ => WakesUp)
    private def wakesUp[_: P]: P[Event] = P("Guard #"~ int ~" begins shift").map(id => ShiftStarts(id))

    // 1518-11-01
    private def date[_: P]: P[LocalDate] = P(int ~ "-" ~ int ~ "-" ~ int)
      .map { case (year, month, day) => LocalDate.of(year, month, day) }

    // 00:53
    private def time[_: P]: P[LocalTime] = P(int ~ ":" ~ int)
      .map { case (h, m) => LocalTime.of(h, m) }

    // [1518-11-01 00:53]
    private def dateTime[_: P]: P[LocalDateTime] = P("[" ~ date ~ " " ~ time ~ "]").map { case (date, time) => LocalDateTime.of(date, time)}



    private def int[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  }

}
