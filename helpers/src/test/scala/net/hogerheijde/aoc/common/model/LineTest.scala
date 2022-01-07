package net.hogerheijde.aoc.common.model

import org.scalatest.matchers.HavePropertyMatchResult
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait LineMatchers {
  def collisionWith(x: Int, y: Int): HavePropertyMatcher[Line, Coordinate] = {
    val point = Coordinate(x, y)
    new HavePropertyMatcher[Line, Coordinate] {
      def apply(line: Line): HavePropertyMatchResult[Coordinate] =
        HavePropertyMatchResult(
          line.collision(point),
          "collision",
          point,
          point
        )
    }
  }
  def collisionWith(point: Coordinate): HavePropertyMatcher[Line, Coordinate] =
    new HavePropertyMatcher[Line, Coordinate] {
      def apply(line: Line): HavePropertyMatchResult[Coordinate] =
        HavePropertyMatchResult(
          line.collision(point),
          "collision",
          point,
          point
        )
    }
}

// Make them easy to import with:
// import CustomMatchers._
object LineMatchers extends LineMatchers


class LineTest extends AnyWordSpec with Matchers with LineMatchers {

  "line" should {
    "bugfixes" in {
      Line((0, 0), (8, 8)) should have(collisionWith(3, 3))
      Line((5, 5), (8, 2)) should have(collisionWith(7, 3))
    }

    "know point is on line" in {
      Range.inclusive(-10, 10).foreach { i =>
        Line(Coordinate(0, -10), Coordinate(0, 10)) should have (collisionWith(Coordinate(0, i)))
        Line(Coordinate(-10, 0), Coordinate(10, 0)) should have (collisionWith(Coordinate(i, 0)))
        Line(Coordinate(-10, -10), Coordinate(10, 10)) should have (collisionWith(Coordinate(i, i)))
      }
    }

    "know point is not on line" in {
      val coordinatesWithoutDiagonal = Range.inclusive(-10, 10).combinations(2).map {
        case Seq(first, second) => (first, second)
      }.filterNot(c => c._1 == c._2)
      coordinatesWithoutDiagonal.foreach { case (x, y) =>
        Line(Coordinate(-10, -10), Coordinate(10, 10)) should not have collisionWith(Coordinate(x, y))
      }
    }
  }
}
