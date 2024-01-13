package net.hogerheijde.aoc2022

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2022.Day9
import net.hogerheijde.aoc2022.Day9.Direction.Up
import net.hogerheijde.aoc2022.Day9.Direction.Down
import net.hogerheijde.aoc2022.Day9.Direction.Right
import net.hogerheijde.aoc2022.Day9.Direction.Left
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """
      |R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      |""".stripMargin


  "parser" should {
    "parse example" in {
      Day9.parse(exampleInput) should be (
        Seq(
          Right(4),
          Up(4),
          Left(3),
          Down(1),
          Right(4),
          Down(1),
          Left(5),
          Right(2),
        )
      )

    }
  }

  "Day 9" should {
    "solve part 1" in {
      Day9.part1(Day9.parse(exampleInput)) should be(13)
    }
  }


  "Coordinate" should {

    "diff" in {
      for {
        h <- Range(-10, 10)
        v <- Range(-10, 10)
      } yield {
        Coordinate(0, 0) diff Coordinate(v, h) should be((v, h))
        Coordinate(2, 3) diff Coordinate(v, h) should be((v - 2, h - 3))
        Coordinate(-2, -1) diff Coordinate(v, h) should be((v + 2, h + 1))
      }
    }

    "stay when moveTowards is connected" in {
      for {
        startH <- Range(-10, 10)
        startV <- Range(-10, 10)
      } yield {
        val current = Coordinate(startH, startV)
        current.moveTowards(current) should be(current)
        current.moveTowards(current.leftUp) should be(current)
        current.moveTowards(current.up) should be(current)
        current.moveTowards(current.rightUp) should be(current)
        current.moveTowards(current.left) should be(current)
        current.moveTowards(current.right) should be(current)
        current.moveTowards(current.leftDown) should be(current)
        current.moveTowards(current.down) should be(current)
        current.moveTowards(current.rightDown) should be(current)
      }
    }
    "actually move when moveTowards is > 1 step away." in {
      for {
        startH <- Range(-10, 10)
        startV <- Range(-10, 10)
      } yield {
        val current = Coordinate(startH, startV)

        current.moveTowards(current.up.up.left.left) should be(current.leftUp)
        current.moveTowards(current.up.up.left) should be(current.leftUp)
        current.moveTowards(current.up.up) should be(current.up)
        current.moveTowards(current.up.up.right) should be(current.rightUp)

        current.moveTowards(current.up.left.left) should be(current.leftUp)
        current.moveTowards(current.up.right.right) should be(current.rightUp)

        current.moveTowards(current.left.left) should be(current.left)
        current.moveTowards(current.right.right) should be(current.right)

        current.moveTowards(current.down.left.left) should be(current.leftDown)
        current.moveTowards(current.down.right.right) should be(current.rightDown)

        current.moveTowards(current.down.down.left.left) should be(current.leftDown)
        current.moveTowards(current.down.down.left) should be(current.leftDown)
        current.moveTowards(current.down.down) should be(current.down)
        current.moveTowards(current.down.down.right) should be(current.rightDown)
        current.moveTowards(current.down.down.right.right) should be(current.rightDown)
      }
    }
  }


}
