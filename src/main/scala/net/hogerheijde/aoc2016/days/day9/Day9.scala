package net.hogerheijde.aoc2016.days.day9

object Day9 {

  def processPt1(input: String): Int = {
    lengthOf(input.iterator)
  }


  def processExpansion(iterator: Iterator[Char]): (Int, Int) = {
    parseCompression(iterator.takeWhile(_ != ')').mkString)
  }

  def processPt2(input: String): Long = {
    lengthOf2(input.iterator)
  }

  def lengthOf2(input: Iterator[Char]): Long = {
    if(input.isEmpty) { 0 } else {
      val unexpanded: Long = input.takeWhile(_ != '(').length

      val (size, times) = processExpansion(input)
      val toExpand = input.take(size)

      val leftOver = input.drop(size)

      val expandedLength = lengthOf2(toExpand)
      val leftOverLength = lengthOf2(leftOver)
      unexpanded + (times * expandedLength) + leftOverLength
    }
  }


  def lengthOf(input: Iterator[Char]): Int = {
    if (input.isEmpty) { 0 } else {
      val unexpanded = input.takeWhile(_ != '(').length

      val (size, times) = processExpansion(input)
      val newIterator = input.drop(size)

      unexpanded + (size * times) + lengthOf(newIterator)
    }
  }


  val regex = "([0-9]+)x([0-9]+)".r
  def parseCompression(input: String): (Int, Int) = {
    input match {
      case regex(size, times) => (size.toInt, times.toInt)
      case _ => (0, 0) // This is incorrect behaviour!
    }
  }

}
