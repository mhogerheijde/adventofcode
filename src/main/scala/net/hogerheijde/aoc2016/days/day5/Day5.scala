package net.hogerheijde.aoc2016.days.day5

import net.hogerheijde.aoc2016.days.RunnableDay

import scala.annotation.tailrec

object Day5 extends RunnableDay {

  def run(): Unit = {
    val code = Day5.crackP("ugkcyxxp")
    println(s"Day 05 - pt1: $code (expect d4cd2ee1)")
    val code2 = Day5.crack2P("ugkcyxxp")
    println(s"Day 05 - pt2: $code2 (expect f2c730e5)")
  }

  def crack(doorId: String): String = {
    val start = ("", 0)

    val (password, lastIndex) = Range(0, 8).foldLeft(start) { case ((partialPassword, index), _) =>
      val (nextChar, newIndex) = nextDigit(doorId, index)
      (partialPassword + nextChar, newIndex + 1)
    }
    password
  }

  def crack2P(doorId: String): String = {
    val starvalues = Seq(
      Range(0, 2000000),
      Range(2000000, 4000000),
      Range(4000000, 6000000),
      Range(6000000, 8000000),
      Range(8000000, 10000000),
      Range(10000000, 12000000),
      Range(12000000, 14000000),
      Range(14000000, 18000000),
      Range(18000000, 20000000),
      Range(20000000, 22000000),
      Range(22000000, 24000000),
      Range(24000000, 26000000),
      Range(26000000, 28000000),
      Range(28000000, 30000000),
      Range(30000000, 32000000),
      Range(32000000, 34000000),
      Range(34000000, 36000000)
    )
    val result = starvalues.par.flatMap { case range =>
      val password = range.flatMap { index =>
        if (index % 250000 == 0) { print("."); Console.flush }
        md5(doorId + index.toString) match {
          case want if want.startsWith("00000") =>
            val pos = want.slice(5, 6).head.toChar
            if (pos >= '0' && pos <= '7') {
              val position = pos.toString.toInt
              val char = want.slice(6, 7).head.toChar
              val temp = position + "|" + ("_" * position) + char + ("_" * (7-position)) + s"|$index"
              println("\n" + temp)
              Console.flush()
              Some( (char, position) )
            } else {
              None
            }
          case _ => None
        }
      }
      password
    }
    result.foldLeft("________") { case (result, (nextChar, nextPos)) =>
      if(result.charAt(nextPos) == '_') { result.updated(nextPos, nextChar) } else { result }
    }
  }

  def crackP(doorId: String): String = {
//    val starvalues = Seq(Range(0, 200000000), Range(200000000, 400000000), Range(400000000, 600000000), Range(600000000, 800000000), Range(800000000, 1000000000))
    val starvalues = Seq(Range(0, 2000000), Range(2000000, 4000000), Range(4000000, 6000000), Range(6000000, 8000000), Range(8000000, 10000000), Range(10000000, 12000000))
    val result = starvalues.par.map { case range =>
      val password = range.flatMap { index =>
        if (index % 250000 == 0) { print("."); Console.flush }
        md5(doorId + index.toString) match {
          case want if want.startsWith("00000") =>
            val char = want.slice(5, 6).head.toChar
            print(char); Console.flush()
            Some(char)
          case _ => None
        }
      }
      password.mkString
    }
    result.mkString.take(8)
  }

  @tailrec
  def nextDigit(doorId: String, index: Int): (Char, Int) = {
    if (index % 250000 == 0) { print(s"."); Console.flush }
    md5(doorId + index.toString) match {
      case want if want.startsWith("00000") =>
        val char = want.slice(5, 6).head.toChar
        println(char)
        (char, index)
      case _ => nextDigit(doorId, index+1)
    }
  }

  def md5(text: String) : String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}

}
