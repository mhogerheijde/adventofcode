package net.hogerheijde.aoc2015.day4

import java.security.MessageDigest

import net.hogerheijde.aoc2015.util.Day

object Day4 extends Day[String, Int, Int] {
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 4"
  override def parse: String => String = identity

  override def part1(input: String): Int = {
    val x = Stream.from(1).collectFirst { case q if hash(input)(q).startsWith("00000") => q }
    x.get
  }

  override def part2(input: String): Int = {
    val x = Stream.from(1).collectFirst { case q if hash(input)(q).startsWith("000000") => q }
    x.get
  }

  def hash(secret: String)(number: Int): String = {
    val toHash = s"$secret$number"
    val result = MessageDigest.getInstance("MD5").
      digest(toHash.getBytes("UTF-8")).
      map("%02x".format(_)).
      mkString

    println(s"Hashing $toHash: $result")

    result
  }
}
