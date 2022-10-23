package net.hogerheijde.aoc.grid

import net.hogerheijde.aoc.common.model.Coordinate2


trait Kernel[T] {
  def locations: Map[Coordinate2, T]
  def valueAt(c: Coordinate2): Option[T]
}

case class Square[T](locations: Map[Coordinate2, T]) extends Kernel[T] {
  override def valueAt(c: Coordinate2): Option[T] = locations.get(c)
}
object Square {
  def kernelAt[T](grid: Grid[T], c: Coordinate2): Square[T] = {
    val coordinates = Set(
      c.up.left, c.up, c.up.right,
      c.left, c, c.right,
      c.down.left, c.down, c.down.right,
    )
    Square(grid.subset(coordinates))
  }
}

class Grid[T](grid: Map[Coordinate2, T]) {

  def subset(coordinates: Set[Coordinate2]): Map[Coordinate2, T] = grid.view.filterKeys(coordinates).toMap

  def update[R](kernelFinder: (Grid[T], Coordinate2) => Kernel[T])
      (f: Kernel[T] => R): Grid[R] = {
    new Grid(grid.view.map { case (c, _) => c -> f(kernelFinder(this, c)) }.toMap)
  }

  override def toString: String = {
    val b = new StringBuffer
    val maxX = grid.keySet.maxBy { c => c.x }.x
    val maxY = grid.keySet.maxBy { c => c.y }.y
    val minX = grid.keySet.minBy { c => c.x }.x
    val minY = grid.keySet.minBy { c => c.y }.y

    // y vertical axis, down is positive
    for { y <- (minY to maxY).reverse} yield {
      // x horizontal axis, right is positive
      for { x <- minX to maxX } yield {
        b.append(grid(Coordinate2(x = x, y = y)))
      }
      b.append("\n")
    }
    b.toString
  }
}
