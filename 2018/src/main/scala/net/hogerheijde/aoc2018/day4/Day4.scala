package net.hogerheijde.aoc2018.day4

import java.time.LocalDateTime
import java.time.ZoneOffset

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import net.hogerheijde.aoc2018.day4.Model.Event
import net.hogerheijde.aoc2018.day4.Model.FallsAsleep
import net.hogerheijde.aoc2018.day4.Model.Interval
import net.hogerheijde.aoc2018.day4.Model.Nightshift
import net.hogerheijde.aoc2018.day4.Model.NightshiftMapping
import net.hogerheijde.aoc2018.day4.Model.ShiftStarts
import net.hogerheijde.aoc2018.day4.Model.WakesUp


object Day4 extends Day2018[NightshiftMapping, Int, Int] {

  override def name: String = "Day 4"
  override def parse(input: String): NightshiftMapping = {
    val eventStream = Parser.standardLineSplit(input)
      .flatMap(a => Event.parse(a))
      .sortBy(_._1.toEpochSecond(ZoneOffset.UTC))



    // TODO might have used fastparse here to help, doing things manually :)
    val processed = eventStream.foldLeft((Map.empty[(Int, LocalDateTime), Nightshift], Option.empty[Int], Option.empty[LocalDateTime], Option.empty[LocalDateTime])) {
      case ((result, currentGuardId, guardShiftStarts, fallsAsleep), (currentDateTime, currentEvent)) =>
      currentEvent match {
        case ShiftStarts(guard) =>
          (result, Some(guard), Some(currentDateTime), None)
        case FallsAsleep =>
          (result, currentGuardId, guardShiftStarts, Some(currentDateTime))
        case WakesUp =>
          val guardId = currentGuardId.get
          val startOfShift = guardShiftStarts.get
          // Should have a guard ID here...
          val guardKey = (guardId, startOfShift) // Should have started a shift
          val newSleepyTimes = Interval(fallsAsleep.get, currentDateTime)
          // can't wake up if not fallen asleep.
          val theGuard = result.getOrElse(guardKey, Nightshift(guardId, startOfShift, Seq()))

          val newGuard = theGuard.copy(sleepyTimes = theGuard.sleepyTimes :+ newSleepyTimes)

          (result.updated(guardKey, newGuard), currentGuardId, guardShiftStarts, None)
      }
    }

    processed._1
  }
  override def part1(input: Map[(Int, LocalDateTime), Nightshift]): Int = {
    val g = sleepiestGuard(input)
    g * sleepiestMinuteByGuard(input, g)
  }
  override def part2(input: Map[(Int, LocalDateTime), Nightshift]): Int = {
    val sleepy = sleepiestMinute(input)
    sleepy._1 * sleepy._2
  }

  def sleepiestGuard(mapping: NightshiftMapping): Int = {
    val x: (Int, Long) = mapping.mapValues(nightShift => nightShift.sleepMinutes).toIndexedSeq.groupBy {
      case ((id, _), _) => id
    }.mapValues(_.map(_._2).sum).maxBy(_._2)
    x._1
  }

  def sleepiestMinuteByGuard(mapping: NightshiftMapping, guardId: Int): Int = {
    val sleeps = mapping.filterKeys(_._1 == guardId).toIndexedSeq.flatMap { case (_, shift) =>
      shift.sleepyTimes
    }

    val histogram = Range(0, 59).map(minute =>
      (minute, sleeps.count(_.contains(minute)))
    )

    histogram.maxBy(_._2)._1
  }

  def histogram(sleeps: IndexedSeq[Interval]): IndexedSeq[(Int, Int)] = {
    Range(0, 59).map(minute =>
      (minute, sleeps.count(_.contains(minute)))
    )
  }

  def sleepiestMinute(mapping: NightshiftMapping): (Int, Int) = {
    val sleepsByGuard = mapping.toIndexedSeq
      .map { case ((id, _), shift) =>  (id, shift.sleepyTimes) }
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))

    val sleepiestMinuteByGuard = sleepsByGuard
      .mapValues(q => histogram(q))
      .mapValues(q => q.maxBy(_._2))

    val sleepiestGuardMinute = sleepiestMinuteByGuard.maxBy { case (_, (_, countForMinute)) => countForMinute}

    (sleepiestGuardMinute._1, sleepiestGuardMinute._2._1)
  }



}
