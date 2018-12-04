package net.hogerheijde.aoc2018.day4

import java.time.LocalDateTime

import net.hogerheijde.aoc2018.day4.Model.Interval
import net.hogerheijde.aoc2018.day4.Model.Nightshift
import net.hogerheijde.aoc2018.day4.Model.NightshiftMapping
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day4Test extends WordSpec with Matchers {

  val shiftStart_g10_1 = LocalDateTime.parse("1518-11-01T00:00")
  val shiftStart_g10_2 = LocalDateTime.parse("1518-11-03T00:05")
  val shiftStart_g99_1 = LocalDateTime.parse("1518-11-01T23:58")
  val shiftStart_g99_2 = LocalDateTime.parse("1518-11-04T00:02")
  val shiftStart_g99_3 = LocalDateTime.parse("1518-11-05T00:03")

  val exampleNightshift: NightshiftMapping = Map(
        (10, shiftStart_g10_1) -> Nightshift(10, shiftStart_g10_1, Seq(
          Interval("1518-11-01T00:05", "1518-11-01T00:25"),
          Interval("1518-11-01T00:30", "1518-11-01T00:55"),
        )),
        (10, shiftStart_g10_2) -> Nightshift(10, shiftStart_g10_2, Seq(
          Interval("1518-11-03T00:24", "1518-11-03T00:29"),
        )),
        (99, shiftStart_g99_1) -> Nightshift(99, shiftStart_g99_1, Seq(
          Interval("1518-11-02T00:40", "1518-11-02T00:50"),
        )),
        (99, shiftStart_g99_2) -> Nightshift(99, shiftStart_g99_2, Seq(
          Interval("1518-11-04T00:36", "1518-11-04T00:46"),
        )),
        (99, shiftStart_g99_3) -> Nightshift(99, shiftStart_g99_3, Seq(
          Interval("1518-11-05T00:45", "1518-11-05T00:55"),
        )),
      )


  "Day 4" should {
    "parse input" in {
      val in = """[1518-11-01 00:00] Guard #10 begins shift
                 |[1518-11-01 00:05] falls asleep
                 |[1518-11-01 00:25] wakes up
                 |[1518-11-01 00:30] falls asleep
                 |[1518-11-01 00:55] wakes up
                 |[1518-11-01 23:58] Guard #99 begins shift
                 |[1518-11-02 00:40] falls asleep
                 |[1518-11-02 00:50] wakes up
                 |[1518-11-03 00:05] Guard #10 begins shift
                 |[1518-11-03 00:24] falls asleep
                 |[1518-11-03 00:29] wakes up
                 |[1518-11-04 00:02] Guard #99 begins shift
                 |[1518-11-04 00:36] falls asleep
                 |[1518-11-04 00:46] wakes up
                 |[1518-11-05 00:03] Guard #99 begins shift
                 |[1518-11-05 00:45] falls asleep
                 |[1518-11-05 00:55] wakes up""".stripMargin

      Day4.parse(in) should be (exampleNightshift)
    }

    "find sleepiest guard" in  {
      Day4.sleepiestGuard(exampleNightshift) should be (10)
    }

    "find the sleepiest minute for specific guard" in {
      Day4.sleepiestMinuteByGuard(exampleNightshift, 10) should be (24)
    }

    "find the sleepiest guard-minute" in {
      Day4.sleepiestMinute(exampleNightshift) should be (99, 45)
    }

  }

}
