package net.hogerheijde.aoc2016.days.day13

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GridTest extends FlatSpec with Matchers {

  "Grid" should "convert numbers into binary string representation" in {

    Grid.toBin(0) should be("0")
    Grid.toBin(1) should be("1")
    Grid.toBin(2) should be("10")
    Grid.toBin(3) should be("11")
    Grid.toBin(4) should be("100")
    Grid.toBin(5) should be("101")
    Grid.toBin(6) should be("110")
    Grid.toBin(7) should be("111")
    Grid.toBin(8) should be("1000")
    Grid.toBin(9) should be("1001")
    Grid.toBin(10) should be("1010")
    Grid.toBin(254) should be("11111110")
    Grid.toBin(255) should be("11111111")
    Grid.toBin(256) should be("100000000")
    Grid.toBin(510) should be("111111110")
    Grid.toBin(511) should be("111111111")
    Grid.toBin(512) should be("1000000000")


  }

  it should "create a wall" in {

    new Grid(10).generateTile(1, 0) should be (Wall(1, 0))
    new Grid(10).generateTile(3, 0) should be (Wall(3, 0))
    new Grid(10).generateTile(4, 0) should be (Wall(4, 0))
    new Grid(10).generateTile(9, 6) should be (Wall(9, 6))
  }

  it should "create a Hall" in {

    new Grid(10).generateTile(0, 0) should be (Hall(0, 0))
    new Grid(10).generateTile(0, 1) should be (Hall(0, 1))
    new Grid(10).generateTile(1, 1) should be (Hall(1, 1))
  }

  it should "output in a grid" in {
    val expect = """  0123456789
                   |0 .#.####.##
                   |1 ..#..#...#
                   |2 #....##...
                   |3 ###.#.###.
                   |4 .##..#..#.
                   |5 ..##....#.
                   |6 #...##.###""".stripMargin


    new Grid(10).output(10, 7) should be (expect)

  }

}
