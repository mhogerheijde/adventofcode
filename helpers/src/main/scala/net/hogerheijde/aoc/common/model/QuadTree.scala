package net.hogerheijde.aoc.common.model

import net.hogerheijde.aoc.common.model.QuadTree.Subtree



case class QuadTree[T](
    width: Int,
    height: Int,
    maxValues: Int,
    value: IndexedSeq[(Coordinate, T)],
    subtree: Option[Subtree[T]]) {

  def add(newValue: (Coordinate, T)): QuadTree[T] = {
    subtree match {
      case None =>
        if (this.value.length < maxValues) {
          this.copy(value = this.value :+ newValue)
        } else {
          val newSubtree = Subtree.forSize[T](width, height)
          this.copy(subtree = Some(newSubtree.add(newValue)))
        }

      case Some(subtree) =>
        this.copy(subtree = Some(subtree.add(newValue)))
    }
  }

}

object QuadTree {
  protected case class Subtree[T](q1: QuadTree[T], q2: QuadTree[T], q3: QuadTree[T], q4: QuadTree[T]) {
    def add(newValue: (Coordinate, T)): Subtree[T] = ???
  }
  object Subtree {
    def forSize[T](width: Int, height: Int): Subtree[T] = {
      if(width == 2) {

      }

      ???
    }
  }

}
