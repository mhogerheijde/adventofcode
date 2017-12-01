package net.hogerheijde.aoc2015.day2

case class Box(length: Int, width: Int, height: Int) {

  val wrappingPaper: Int = {
    val sides = Seq(length * width, width * height, height * length)
    sides.min + sides.foldLeft(0) { (total, side) => total + 2*side}
  }

  val ribbon: Int = {
    val dimensions = Seq(length, width, height).sorted
    val around = dimensions.init.foldLeft(0) { (total, side) => total + 2*side }
    val bow = dimensions.product
    bow + around
  }
}