package net.hogerheijde.aoc2018.day4

import java.time.Duration

import net.hogerheijde.aoc2018.day3.Model.Point
import net.hogerheijde.aoc2018.day4.Model.Interval
import net.hogerheijde.aoc2018.day4.Model.Interval
import net.hogerheijde.aoc2018.day4.Model.Nightshift
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class ModelTest extends WordSpec with Matchers {


  def contain(minute: Int): Matcher[Interval] = Matcher { (intervalUnderTest: Interval) =>
    MatchResult(
      intervalUnderTest.contains(minute),
      intervalUnderTest + " does not contain " + minute,
      intervalUnderTest + " does contain " + minute,
    )
  }


  "Interval" should {
    "have correct duration" in {
      Interval("1518-11-01 00:05", "1518-11-01 00:25").sleepMinutes should be (20)
      Interval("1518-11-01 00:30", "1518-11-01 00:55").sleepMinutes should be (25)
    }

    "knows which minute it holds" in {

      val theInterval = Interval("1518-11-01 00:05", "1518-11-01 00:25")

      theInterval should not(contain(3))
      theInterval should not(contain(4))
      theInterval should contain(5)
      theInterval should contain(6)
      theInterval should contain(23)
      theInterval should contain(24)
      theInterval should not(contain(25))
      theInterval should not(contain(26))
    }
  }

  "Nightshift" should {
    "have correct duration" in {
      Nightshift(1, "1518-11-01 00:00", Seq(
        Interval("1518-11-01 00:05", "1518-11-01 00:25"),
        Interval("1518-11-01 00:30", "1518-11-01 00:55"),
      )).sleepMinutes should be (45)
    }
  }
}
