package net.hogerheijde.aoc2017.day3

import scala.annotation.tailrec

import net.hogerheijde.aoc2017.Day2017

object Day3 extends Day2017[Int, Int, Int]{


  def main(args: Array[String]): Unit = run()
  override def name: String = "Day 3"

  override def parse(input: String): Int = Integer.parseInt(input)

  override def part1(input: Int): Int = {
    val c = coordinateOf(input)
    c.horizontal + c.vertical // Manhattan distance of a coordinate
  }


  override def part2(input: Int): Int = {

    val start = Map(Coordinate.Center -> 1)

    recurse((start, Coordinate.Center), input)

  }

  @tailrec
  def recurse(state: (Map[Coordinate, Int], Coordinate), input: Int): Int = {
    val nextGrid = increase(state._1, state._2)
    val currentValue = nextGrid._1(nextGrid._2)
    if (currentValue > input) {
      currentValue
    } else {
      recurse(nextGrid, input)
    }
  }

  def increase(grid: Map[Coordinate, Int], currentCoordinate: Coordinate): (Map[Coordinate, Int], Coordinate) = {
    val nextCoordinate = currentCoordinate.next
    val nextValue = nextCoordinate.neigbors.flatMap(grid.get).sum
    (grid.updated(nextCoordinate, nextValue), nextCoordinate)
  }



  def coordinateOf(input: Int): Coordinate = {
    if (input == 1) { Coordinate.Center } else {
      Coordinate(ringOf(input), positionOf(input))
    }
  }

  def positionOf(input: Int): Int = {
    val power = ringPower(input)
    val cornerOffset = (Math.pow(power, 2) - input).toInt % (power - 1)
    Math.abs(cornerOffset - ringOf(input))
  }

  def ringPower(input: Int): Int = {
    val root = Math.sqrt(input).ceil.toInt
    if (root % 2 == 0) { root + 1 } else { root }

  }

  def ringOf(input: Int): Int = {
    (ringPower(input)/ 2.0).floor.toInt
  }
//
//  def nextCoordinate(coordinate: Coordinate): Coordinate = {
//
//    direction match {
//      case Up => {
//        val newCoordinate = coordinate.copy(vertical = coordinate.vertical + 1)
//        if (newCoordinate.isCorner) {
//          (newCoordinate, Left)
//        } else {
//
//        }
//      }
//      case Down => ???
//      case Right => ???
//      case Left => ???
//    }
//
//  }



//  trait Direction
//  case object Up extends Direction
//  case object Down extends Direction
//  case object Left extends Direction
//  case object Right extends Direction



}
