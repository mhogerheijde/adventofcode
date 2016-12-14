package net.hogerheijde.aoc2016.days.day4

import scala.collection.immutable.Map

object Day4 {

  type Code = String
  type SectorId = Int
  type Checksum = String
  type Room = (Code, SectorId, Checksum)
  type Histogram = Map[Char, Int]
  val Histogram = scala.collection.immutable.Map

  val last = 'z'.toInt
  val offset = 'a'.toInt - last - 1 // Use the force to correct for Obi One error

  def process(input: String): Int = {
    val zero = 0
    (input.split("\n") map { parse } filter { correctChecksum }).foldLeft(zero) { case (total, (_, sectorId, _)) =>
      total + sectorId
    }
  }

  def findRoomSector(input: String, roomName: String): Option[SectorId] = {
    val rooms = decryptRooms(input)

    rooms.find(_._1 == roomName).map(_._2)
  }

  def decryptRooms(input: String): IndexedSeq[Room] = {
    input.split("\n") map { parse } filter { correctChecksum } map { decrypt }
  }

  def decrypt(room: Room): Room = {
    val (code, sectorId, checksum) = room
    val shift = sectorId % 26
    val decrypted = (code map {
      case '-' => ' '
      case char =>
        val newChar = char + shift
        if (newChar > last) {
          (offset + newChar).toChar
        } else {
          newChar.toChar
        }
    }).toString
    (decrypted, sectorId, checksum)
  }

  def correctChecksum: Room => Boolean = { case (code, foo, checksum) =>
    createChecksum(histogram(code)).startsWith(checksum)
  }


  def parse(input: String): Room = {
    val parts = input.split("-")
    val code = parts.init.mkString("-")
    val parts2 = parts.last.split("\\[")

    val sector = parts2(0).toInt
    val checksum = parts2(1).dropRight(1)

    (code, sector, checksum)
  }

  def histogram(input: String): Histogram = {
    val default: Histogram = Histogram()
    input.foldLeft(default) { (histogram, char) =>
      if (char == '-') {
        histogram
      } else {
        histogram.updated(char, histogram.getOrElse(char, 0) + 1)
      }
    }
  }

  def createChecksum(histogram: Histogram): Checksum = {
    (histogram.toSeq sortWith { case ((char1, frequency1), (char2, frequency2)) =>
      val frequencyCompare = frequency1.compareTo(frequency2)
      if (frequencyCompare == 0) {
        char1.compareTo(char2) < 0
      } else {
        frequencyCompare > 0
      }
    } map { case (char, freq) =>
      char
    }).mkString
  }

}
