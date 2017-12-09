package net.hogerheijde.aoc2017.day9

import scala.collection.immutable.IndexedSeq

trait Thing {
  def level: Int
}
case class Tokens(chars: List[Char])


case class Group(level: Int, groups: IndexedSeq[Thing]) extends Thing {

  override def toString: String = {
    val subGroups = if (groups.nonEmpty) { groups.mkString(" | ", ", ", "") } else { "" }
    s"Group($level$subGroups)"
  }

}
object Group {
  def apply(level: Int, subThings: Thing*): Group = new Group(level, subThings.toIndexedSeq)
  def apply(level: Int, subThing: Thing): Group = Group(level, IndexedSeq(subThing))
  //  def apply(subThings: Option[IndexedSeq[Thing]]): Group = Group(subThings.getOrElse(IndexedSeq()))
  def apply(level: Int): Group = Group(level, IndexedSeq())
}
case class Garbage(level: Int, count: Int) extends Thing