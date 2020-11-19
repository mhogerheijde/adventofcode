package net.hogerheijde.aoc2015.day6

import scala.collection.immutable.Seq
import scala.util.Try

import net.hogerheijde.aoc2015.day6.State.Off
import net.hogerheijde.aoc2015.day6.State.On
import net.hogerheijde.aoc2015.util.Day
import scala.collection.immutable.Set
import scala.collection.immutable.Map

object IntMatcher {
  def unapply(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
}


case class Coordinate(x: Int, y: Int) {
  override def toString: String = s"$x,$y"
}
object Coordinate {
  def unapply(value: String): Option[Coordinate] = value.split(",").toSeq match {
    case Seq(IntMatcher(x), IntMatcher(y)) => Some(Coordinate(x, y))
    case _ => None
  }
}

case class Range(start: Coordinate, end: Coordinate) {
  override def toString: String = s"$start through $end"
  def iterator: Iterator[Coordinate] = {
    Iterator.unfold(start) { previous =>
      previous match {
        case Coordinate(x, y) if x > end.x || y > end.y =>
          None
        case Coordinate(x, y) if x < end.x =>
          Some( (previous, Coordinate(x+1, y)) )
        case Coordinate(x, y) if x == end.x =>
          Some( (previous, Coordinate(start.x, y+1)) )
      }
    }
  }
}
object Range {
  def unapply(q: String): Option[Range] = q match {
    case s"${Coordinate(from)} through ${Coordinate(to)}" => Some(Range(from, to))
    case _ => None
  }
}

sealed trait Instruction {
  def range: Range
}
object Instruction {

  def unapply(input: String): Option[Instruction] = input match {
    case Instruction.On(on) => Some(on)
    case Instruction.Off(off) => Some(off)
    case Instruction.Toggle(toggle) => Some(toggle)
    case _ => None
  }

  case class On(range: Range) extends Instruction {
    override def toString: String = s"turn on $range"
  }

  object On {
    def unapply(input: String): Option[On] = input match {
      case s"turn on ${Range(range)}" => Some(On(range))
      case _ => None
    }
  }

  case class Off(range: Range) extends Instruction {
    override def toString: String = s"turn off $range"
  }

  object Off {
    def unapply(input: String): Option[Off] = input match {
      case s"turn off ${Range(range)}" => Some(Off(range))
      case _ => None
    }
  }

  case class Toggle(range: Range) extends Instruction {
    override def toString: String = s"toggle $range"
  }

  object Toggle {
    def unapply(input: String): Option[Toggle] = input match {
      case s"toggle ${Range(range)}" => Some(Toggle(range))
      case _ => None
    }
  }
}
sealed trait State {
  def transpose(i: Instruction): State
}
object State {
  object On extends State {
    def transpose(i: Instruction): State = i match {
      case _: Instruction.On => On
      case _: Instruction.Off => Off
      case _: Instruction.Toggle => Off
    }
  }
  object Off extends State {
    def transpose(i: Instruction): State = i match {
      case _: Instruction.On => On
      case _: Instruction.Off => Off
      case _: Instruction.Toggle => Off
    }
  }
}

case class Grid private(state: Set[Coordinate]) {
  def update(coordinate: Coordinate, instruction: Instruction): Grid = {
     Grid(instruction match {
       case _: Instruction.On => state + coordinate
       case _: Instruction.Off => state - coordinate
       case _: Instruction.Toggle => if (state.contains(coordinate)) { state - coordinate } else { state + coordinate }
    })
  }

  def count(): Int = state.size
}
object Grid {
  def apply(): Grid = Grid(Set.empty)
}

case class Grid2 private(state: Map[Coordinate, Int]) {
  def update(coordinate: Coordinate, instruction: Instruction): Grid2 = {
    Grid2(instruction match {
      case _: Instruction.On => state + ((coordinate, state.get(coordinate).map(_ + 1).getOrElse(1)))
      case _: Instruction.Off => state + ((coordinate, state.get(coordinate).map { v => if (v == 0) { 0 } else {v - 1}}.getOrElse(0)))
      case _: Instruction.Toggle => state + ((coordinate, state.get(coordinate).map(_ + 2).getOrElse(2)))
    })
  }
  def count(): Int = state.map { case (_, intensity) => intensity }.sum
}
object Grid2 {
  def apply(): Grid2 = Grid2(Map.empty)
}


object Day6 extends Day[Seq[Instruction], Int, Int] {
  def main(args: Array[String]): Unit = run()

  override def name: String = "Day 6"
  override def parse: String => Seq[Instruction] = in => {
    in.linesIterator.toSeq.map { line => line match {
        case Instruction(i) => i
        case s => throw new Exception(s"Could not parse $s")
      }
    }
  }

  override def part1(input: Seq[Instruction]): Int = {
    val result = input.foldLeft(Grid()) { case (grid, instruction) =>
      instruction.range.iterator.foldLeft(grid) { (grid, coordinate) =>
        grid.update(coordinate, instruction)
      }
    }

    result.count()
  }

  override def part2(input: Seq[Instruction]): Int = {
      val result = input.foldLeft(Grid2()) { case (grid, instruction) =>
        instruction.range.iterator.foldLeft(grid) { (grid, coordinate) =>
          grid.update(coordinate, instruction)
        }
      }

      result.count()
  }
}
