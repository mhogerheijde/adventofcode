package net.hogerheijde.aoc2016.days.day7
import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay

import scala.collection.immutable.IndexedSeq

object Day7 extends RunnableDay {

  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day7.input")

    val amount = Day7.processPt1(input)
    println(s"Day 07 - pt1: $amount (expect 110)")
    val amount2 = Day7.processPt2(input)
    println(s"Day 07 - pt2: $amount2 (expect 242)")
  }

  type IP = (IndexedSeq[String], IndexedSeq[String])
  val IP = scala.Tuple2

  def processPt1(input: String): Int = {
    val ips = input.split("\n").toIndexedSeq.map(parseIp)
    ips.filter(supportsTLS).size
  }

  def processPt2(input: String): Int = {
    val ips = input.split("\n").toIndexedSeq.map(parseIp)
    ips.filter(supportsSSL).size
  }

  def supportsTLS(ip: IP): Boolean = {
    ip._1.exists(containsPattern) && !ip._2.exists(containsPattern)
  }

  def supportsSSL(ip: IP): Boolean = {
    val antiPat = antiPatterns(ip._1)
    ip._2 exists { item =>
      antiPat.exists( item.contains(_) )
    }
  }

  def antiPatterns(list: IndexedSeq[String]): IndexedSeq[String] = {
    list flatMap { item =>
      item.iterator.sliding(3, 1) flatMap { window =>
        val (one, two, three) = (window(0), window(1), window(2))
        if (one == three && one != two) {
          Some(two.toString + one + two)
        } else {
          None
        }
      }
    }
  }


  def containsPattern(string: String): Boolean = {
    string.iterator.sliding(4, 1) exists { window =>
      window(0) == window(3) && window(1) == window(2) && window(0) != window(1)
    }
  }



  def parseIp(input: String): IP = {

    val start: IP = (IndexedSeq(), IndexedSeq())
    val tempPart: Either[String, String] = input.headOption match {
      case Some('[') => Right("")
      case _ => Left("")
    }
    val (ip, lastPart) = input.foldLeft( (start, tempPart) ) { case ((acc, partial), nextChar) =>
      nextChar match {
        case '[' =>
          (acc.copy(_1 = acc._1 :+ partial.left.get), Right(""))
        case ']' =>
          (acc.copy(_2 = acc._2 :+ partial.right.get), Left(""))
        case c =>
          val t = partial match {
            case Right(s) => Right(s + c)
            case Left(s) => Left(s + c)
          }
          (acc, t)
      }
    }

    lastPart match {
      case Right(s) => ip.copy(_2 = ip._2 :+ s)
      case Left(s) => ip.copy(_1 = ip._1 :+ s)
    }


  }

}
