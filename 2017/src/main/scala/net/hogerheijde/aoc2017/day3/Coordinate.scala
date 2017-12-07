package net.hogerheijde.aoc2017.day3

import scala.collection.immutable.IndexedSeq

case class Coordinate(horizontal: Int, vertical: Int) {
  val isTopRightCorner: Boolean = horizontal == vertical && horizontal > 0
  val isTopLeftCorner: Boolean = horizontal * -1 == vertical && horizontal < 0
  val isBottomLeftCorner: Boolean = horizontal == vertical && horizontal < 0
  val isBottomRightCorner: Boolean = horizontal * -1 == vertical && horizontal > 0
  val isCenter: Boolean = horizontal == 0 && vertical == 0

  val isLeftOfTopRightCorner: Boolean = Math.abs(horizontal) < vertical && vertical > 0
  val isBelowTopLeftCorner: Boolean = Math.abs(horizontal) > Math.abs(vertical) && horizontal < 0
  val isRightOfBottomLeftCorner: Boolean = Math.abs(horizontal) < Math.abs(vertical) && vertical < 0
  val isAboveBottomRightCorner: Boolean = Math.abs(horizontal) > Math.abs(vertical) && horizontal > 0


  def right: Coordinate = copy(horizontal = horizontal + 1)
  def left: Coordinate = copy(horizontal = horizontal - 1)
  def down: Coordinate = copy(vertical = vertical - 1)
  def up: Coordinate = copy(vertical = vertical + 1)
  def leftUp: Coordinate = copy(horizontal = horizontal - 1, vertical = vertical + 1)
  def leftDown: Coordinate = copy(horizontal = horizontal - 1, vertical = vertical - 1)
  def rightDown: Coordinate = copy(horizontal = horizontal + 1, vertical = vertical - 1)
  def rightUp: Coordinate = copy(horizontal = horizontal + 1, vertical = vertical + 1)

  def neigbors: IndexedSeq[Coordinate] = IndexedSeq(
   right,
   left,
   down,
   up,
   leftUp,
   leftDown,
   rightDown,
   rightUp)


  def next: Coordinate = {
    if (isCenter) { right }
    else if (isTopRightCorner) { left }
    else if (isTopLeftCorner) { down }
    else if (isBottomLeftCorner) { right }
    else if (isBottomRightCorner) { right }
    else if (isLeftOfTopRightCorner) { left }
    else if (isBelowTopLeftCorner) { down }
    else if (isRightOfBottomLeftCorner) { right }
    else if (isAboveBottomRightCorner) { up }
    else { throw new RuntimeException(s"Could not determine next coordinate for $this") }

  }

  override val toString = s"Coordinate(h=$horizontal, v=$vertical)"


}

object Coordinate {
  val Center: Coordinate = Coordinate(0, 0)
}