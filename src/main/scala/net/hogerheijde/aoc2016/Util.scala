package net.hogerheijde.aoc2016

import net.hogerheijde.aoc2016.days.day1.model.Coordinate

import scala.annotation.tailrec

object Util {


//  def getFirstDuplicateLocation(list: IndexedSeq[Coordinate]): Coordinate = {
//
//    val seen: Set[Coordinate] = Set()
//    val candiate: Option[Coordinate] = None
//
//
//    list.foldLeft( (seen, candiate) ) { case ( (seenCoordinates, candiate), nextCoordinate) =>
//      candiate match {
//        case Some(coordinate) =>
//          // we've already found the candidate. Don't know how to shortcut a fold
//          (seenCoordinates, Some(coordinate))
//        case None =>
//
//
//
//          (seenCoordinates, None)
//      }
//    }
//
//
//
//  }

  def readFile(filename: String): String = {
    scala.io.Source.fromResource(filename).getLines().mkString("\n")
  }

  @tailrec
  def getFirstDupe[A](list: IndexedSeq[A]): Option[A] = {
    if (list.isEmpty) {
      None
    } else {
      list.tail.find(_ == list.head) match {
        case None => getFirstDupe(list.tail)
        case x => x
      }
    }

  }
}
