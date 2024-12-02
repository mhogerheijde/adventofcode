package net.hogerheijde.aoc2023

import fastparse.*
import fastparse.CharIn
import fastparse.NoWhitespace.*
import fastparse.P

import java.time.Duration
import scala.annotation.targetName

@main
def main(): Unit =
  val uptimeString1 = " 07:55:03 up 18 days, 14:46,  0 users,  load average: 0.88, 0.59, 0.52"
  val uptimeString2 = " 15:45:54 up  5:58,  1 user,  load average: 2,89, 2,01, 1,38          "
  val uptimeString3 = " 13:27:50 up 2 days, 18 min,  0 users,  load average: 1.13, 0.93, 0.83"

  println(s"$uptimeString1 :: " + Uptime.parse(uptimeString1).get)
  println(s"$uptimeString2 :: " + Uptime.parse(uptimeString2).get)
  println(s"$uptimeString3 :: " + Uptime.parse(uptimeString3).get)


object Uptime:
  def parse(input: String): Option[Duration] =
    fastparse.parse(input, uptime(_)) match
      case Parsed.Success(result, _) => Some(result)
      case Parsed.Failure(_, _, e) =>
        val t = e.trace()
        println(s"Tried to parse the following input, but failed:\n${input}\nReason: $t")
        None


  private def start[$: P]: P[Unit] = P(Start ~ CharIn(" 0123456789:").rep ~ "up" ~ " ".rep)
  private def end[$: P]: P[Unit] = P(AnyChar.rep ~ End)

  private def number[$: P]: P[Long] = P(CharIn("0-9").rep.!).map((x: String) => x.toLong)

  private def hours[$: P]: P[Duration] = P(number  ~ ":" ~ number ~ ",")
    .map { (hours, minutes) => hours.hours + minutes.minutes }
  private def days[$: P]: P[Duration] = P(number ~ " days, ").map { _.days }
  private def minutes[$: P]: P[Duration] = P(number ~ " min, ").map { _.minutes }

  private def daysHours[$: P]: P[Duration] = P(days ~ hours).map(_ + _)
  private def daysMinutes[$: P]: P[Duration] = P(days ~ minutes).map(_ + _)

  private def uptime[$: P]: P[Duration] = P(start ~ (daysHours | daysMinutes | hours) ~ end)


extension(l: Long)
  def hours: Duration = Duration.ofHours(l)
  def minutes: Duration = Duration.ofMinutes(l)
  def days: Duration = Duration.ofDays(l)

extension(d: Duration)
  @targetName("plus")
  def `+`(o: Duration): Duration = d.plus(o)