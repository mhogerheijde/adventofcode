package net.hogerheijde.aoc2017.day2

import scala.collection.immutable.IndexedSeq
import scala.util.Try

case class Sheet(rows: IndexedSeq[Row]) {
  val checksum2: Int = rows.map(_.division.getOrElse(0)).sum
  val checksum: Int = rows.map(_.checksum).sum
}
object Sheet {
  def apply(rows: Row*): Sheet = Sheet(rows.toIndexedSeq)
}

case class Row(cells: IndexedSeq[Cell]) {
  val division: Option[Int] = {
    val results = cells.toSet.subsets(2).flatMap { pairs =>
      pairs.max / pairs.min
    }
    results.toSeq match {
      case Seq() => None
      case divisions => Some(divisions.max)
    }
  }

  val checksum: Int = cells match {
    case Seq() => 0
    case Seq(single) => 0
    case _ => cells.max - cells.min
  }
}

object Row {
  def fromLine(input: String): Row = Row(input.split("\t").toIndexedSeq.flatMap(Cell.fromString))
  def apply(values: Int*): Row = new Row(values.toIndexedSeq.map(Cell(_)))
}


case class Cell(value: Int) {
  // scalastyle:off method.name
  def -(other: Cell): Int = value - other.value
  def /(other: Cell): Option[Int] = if(value % other.value == 0) { Some(value / other.value) } else { None }
  // scalastyle:on method.name
}
object Cell {
  implicit def ordering: Ordering[Cell] = Ordering.by(_.value)
  def fromString(input: String): Option[Cell] = Try(Integer.parseInt(input.trim)).map(value => Cell(value)).toOption
}
