package net.hogerheijde.aoc2017.day10

import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2017.day10.State.rotate
import net.hogerheijde.aoc2017.day10.State.rotateLeft

trait State {
  def values: IndexedSeq[Int]
  def position: Int
  def skip: Int

  def unRotate: IndexedSeq[Int] = rotateLeft(values, position)
}
object State {
  def rotate(list: IndexedSeq[Int], amount: Int): IndexedSeq[Int] = {
    if (amount == 0) { list }
    else if (amount < 0) { rotateLeft(list, -1*amount)}
    else { rotateRight(list, amount) }
  }

  def rotateRight(list: IndexedSeq[Int], amount: Int): IndexedSeq[Int] = {
    list.drop(amount) ++ list.take(amount)
  }

  def rotateLeft(list: IndexedSeq[Int], amount: Int): IndexedSeq[Int] = {
    list.drop(list.size - amount) ++ list.take(list.size -amount)
  }
}


case class Ended(values: IndexedSeq[Int], position: Int, skip: Int) extends State {
  override def toString: String = {
    val state = unRotate.zipWithIndex.map { case (value, index) =>
      if (index == position) {
        s"[$value]"
      } else {
        s"$value"
      }
    }.mkString(" ")
    s"Ended($state | $skip)"
  }
}
case class Running(values: IndexedSeq[Int], position: Int, skip: Int, lengths: IndexedSeq[Int]) extends State{

  def next: State = {
    val length = lengths.head

    val reversed = values.take(length).reverse
    val toKeep = values.drop(length)

    val newPosition = (position + length + skip) % values.length
    val newValues = rotate(reversed ++ toKeep, newPosition - position)
    val newSkip = skip + 1
    val newLengths = lengths.drop(1)



    newLengths match {
      case IndexedSeq() => Ended(newValues, newPosition, newSkip)
      case _ => Running(newValues, newPosition, newSkip, newLengths)
    }

  }


  override def toString: String = {
    val state = unRotate.zipWithIndex.map { case (value, index) =>
      if (index == position) {
        s"[$value]"
      } else {
        s"$value"
      }
    }.mkString(" ")
    s"Running($state | $skip | ${lengths.mkString(" ")})"
  }
}