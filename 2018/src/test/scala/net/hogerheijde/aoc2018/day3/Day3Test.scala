package net.hogerheijde.aoc2018.day3

import net.hogerheijde.aoc2018.day3.Model.Fabric
import net.hogerheijde.aoc2018.day3.Model.Point
import net.hogerheijde.aoc2018.day3.Model.Square
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

// scalastyle:off magic.number

class Day3Test extends WordSpec with Matchers {

  def beLeftOf(otherPoint: Point): Matcher[Point] = Matcher { (pointUnderTest: Point) =>
    MatchResult(
      pointUnderTest.isLeftOf(otherPoint),
      pointUnderTest + " is not left of " + otherPoint,
      pointUnderTest + " is left of " + otherPoint,
    )
  }

  def beRightOf(otherPoint: Point): Matcher[Point] = Matcher { (pointUnderTest: Point) =>
    MatchResult(
      pointUnderTest.isRightOf(otherPoint),
      pointUnderTest + " is not right of " + otherPoint,
      pointUnderTest + " is right of " + otherPoint,
    )
  }

  def beBelow(otherPoint: Point): Matcher[Point] = Matcher { (pointUnderTest: Point) =>
    MatchResult(
      pointUnderTest.isBelow(otherPoint),
      pointUnderTest + " is not below " + otherPoint,
      pointUnderTest + " is below of " + otherPoint,
    )
  }
  def beAbove(otherPoint: Point): Matcher[Point] = Matcher { (pointUnderTest: Point) =>
    MatchResult(
      pointUnderTest.isAbove(otherPoint),
      pointUnderTest + " is not above " + otherPoint,
      pointUnderTest + " is above of " + otherPoint,
    )
  }


  def contain(point: Point): Matcher[Square] = Matcher { squareUnderTest: Square =>
    MatchResult(
      squareUnderTest.contains(point),
      squareUnderTest + " does not contain " + point,
      squareUnderTest + " does " + point,
    )
  }

  def beLeftOf(otherSquare: Square): Matcher[Square] = Matcher { (squareUnderTest: Square) =>
    MatchResult(
      squareUnderTest.isLeftOf(otherSquare),
      squareUnderTest + " is not left of " + otherSquare,
      squareUnderTest + " is left of " + otherSquare,
    )
  }
  def beRightOf(otherSquare: Square): Matcher[Square] = Matcher { (squareUnderTest: Square) =>
    MatchResult(
      squareUnderTest.isRightOf(otherSquare),
      squareUnderTest + " is not right of " + otherSquare,
      squareUnderTest + " is right of " + otherSquare,
    )
  }

  def beAbove(otherSquare: Square): Matcher[Square] = Matcher { (squareUnderTest: Square) =>
    MatchResult(
      squareUnderTest.isAbove(otherSquare),
      squareUnderTest + " is not above " + otherSquare,
      squareUnderTest + " is above " + otherSquare,
    )
  }

  def beBelow(otherSquare: Square): Matcher[Square] = Matcher { (squareUnderTest: Square) =>
    MatchResult(
      squareUnderTest.isBelow(otherSquare),
      squareUnderTest + " is not below " + otherSquare,
      squareUnderTest + " is below " + otherSquare,
    )
  }

  def intersect(otherSquare: Square): Matcher[Square] = Matcher { squareUnderTest: Square =>

    MatchResult(
      squareUnderTest.intersects(otherSquare),
      squareUnderTest + " does not intersect " + otherSquare,
      squareUnderTest + " does intersect " + otherSquare,
    )
  }

  val exampleSquare1 = Square.parse("#1 @ 1,3: 4x4").get
  val exampleSquare2 = Square.parse("#2 @ 3,1: 4x4").get
  val exampleSquare3 = Square.parse("#3 @ 5,5: 2x2").get
  val exampleSquare4 = Square.parse("#4 @ 4,0: 4x4").get
  val exampleSquare5 = Square.parse("#5 @ 4,0: 3x4").get
  val exampleSquare6 = Square.parse("#6 @ 4,0: 4x3").get


  "Fabric" should {
    "print nicely" in {

      val fabric = Fabric(Set(exampleSquare1, exampleSquare2, exampleSquare3, exampleSquare4))
      println(fabric)

      fabric.toString should be (
        """....4444.
          |...2XXX4.
          |...2XXX4.
          |.11XXXX4.
          |.11XX22..
          |.111133..
          |.111133..
          |.........""".stripMargin
      )


    }
  }


  "Point" should {

    "be ordered" in {
      val p0 = Point(0, 0)
      val p1_0 = Point(1, 0)
      val p0_1 = Point(0, 1)
      Seq(p1_0, p0).sorted should be (Seq(p0, p1_0))
      Seq(p0_1, p0).sorted should be (Seq(p0, p0_1))
      Seq(p0_1, p1_0).sorted should be (Seq(p1_0, p0_1))
      Seq(p1_0, p0_1).sorted should be (Seq(p1_0, p0_1))
      Seq(p1_0, p0_1, p0).sorted should be (Seq(p0, p1_0, p0_1))
      Seq(p0_1, p1_0, p0).sorted should be (Seq(p0, p1_0, p0_1))

      Seq(p0_1, p1_0, p0).max should be (p0_1)
    }



    "be left of" in {
      Point(4, 4) should beLeftOf(Point(5, 3))
      Point(4, 4) should beLeftOf(Point(5, 4))
      Point(4, 4) should beLeftOf(Point(5, 5))
    }
    "be right of" in {
      Point(4, 4) should beRightOf(Point(3, 3))
      Point(4, 4) should beRightOf(Point(3, 4))
      Point(4, 4) should beRightOf(Point(3, 5))
    }
    "be below" in {
      Point(4, 4) should beBelow(Point(3, 3))
      Point(4, 4) should beBelow(Point(4, 3))
      Point(4, 4) should beBelow(Point(5, 3))
    }
    "be above" in {
      Point(4, 4) should beAbove(Point(3, 5))
      Point(4, 4) should beAbove(Point(4, 5))
      Point(4, 4) should beAbove(Point(5, 5))
    }
  }

  "Square" should {
    "be left of" in {
      Square(1, Point(1, 1), Point(2, 2)) should beLeftOf(Square(2, Point(3, 1), Point(4, 1)))
      Square(1, Point(1, 1), Point(2, 2)) should beLeftOf(Square(2, Point(3, 1), Point(4, 2)))
      Square(1, Point(1, 1), Point(2, 2)) should beLeftOf(Square(2, Point(3, 1), Point(4, 3)))
    }

    "not be left of" in {
      Square(1, Point(1, 1), Point(2, 2)) should not(beLeftOf(Square(2, Point(1, 0), Point(2, 2))))
      Square(1, Point(1, 1), Point(2, 2)) should not(beLeftOf(Square(2, Point(1, 1), Point(2, 2))))
      Square(1, Point(1, 1), Point(2, 2)) should not(beLeftOf(Square(2, Point(1, 2), Point(2, 2))))
    }

    "be right of" in {
      Square(1, Point(1, 1), Point(2, 2)) should beRightOf(Square(2, Point(-1, 1), Point(0, 1)))
      Square(1, Point(1, 1), Point(2, 2)) should beRightOf(Square(2, Point(-1, 1), Point(0, 1)))
      Square(1, Point(1, 1), Point(2, 2)) should beRightOf(Square(2, Point(-1, 1), Point(0, 1)))
    }

    "be above" in {
      Square(1, Point(3, 3), Point(4, 4)) should beAbove(Square(2, Point(3, 5), Point(6, 6)))
      Square(1, Point(3, 3), Point(4, 4)) should beAbove(Square(2, Point(4, 5), Point(6, 6)))
      Square(1, Point(3, 3), Point(4, 4)) should beAbove(Square(2, Point(5, 5), Point(6, 6)))
    }

    "be below" in {
      Square(1, Point(3, 3), Point(4, 4)) should beBelow(Square(2, Point(0, 0), Point(1, 2)))
      Square(1, Point(3, 3), Point(4, 4)) should beBelow(Square(2, Point(0, 0), Point(2, 2)))
      Square(1, Point(3, 3), Point(4, 4)) should beBelow(Square(2, Point(0, 0), Point(3, 2)))
    }

    "contain points" in {
      exampleSquare1 should contain(Point(2, 4))
      exampleSquare1 should contain(Point(3, 4))
      exampleSquare1 should contain(Point(4, 4))
      exampleSquare1 should contain(Point(5, 4))

      exampleSquare1 should contain(Point(2, 5))
      exampleSquare1 should contain(Point(3, 5))
      exampleSquare1 should contain(Point(4, 5))
      exampleSquare1 should contain(Point(5, 5))

      exampleSquare1 should contain(Point(2, 6))
      exampleSquare1 should contain(Point(3, 6))
      exampleSquare1 should contain(Point(4, 6))
      exampleSquare1 should contain(Point(5, 6))

      exampleSquare1 should contain(Point(2, 7))
      exampleSquare1 should contain(Point(3, 7))
      exampleSquare1 should contain(Point(4, 7))
      exampleSquare1 should contain(Point(5, 7))
    }

    "not contain points" in {
      val exampleSquare1 = Square(1, Point(1,3), Point(5, 7))

      // Anything directly above
      exampleSquare1 should not(contain(Point(2, 3)))
      exampleSquare1 should not(contain(Point(3, 3)))
      exampleSquare1 should not(contain(Point(4, 3)))
      exampleSquare1 should not(contain(Point(5, 3)))

      // Anything directly below
      exampleSquare1 should not(contain(Point(2, 8)))
      exampleSquare1 should not(contain(Point(3, 8)))
      exampleSquare1 should not(contain(Point(4, 8)))
      exampleSquare1 should not(contain(Point(5, 8)))

      // Anyting directly left of
      exampleSquare1 should not(contain(Point(1, 3)))
      exampleSquare1 should not(contain(Point(1, 4)))
      exampleSquare1 should not(contain(Point(1, 5)))
      exampleSquare1 should not(contain(Point(1, 6)))
      exampleSquare1 should not(contain(Point(1, 7)))
      exampleSquare1 should not(contain(Point(1, 8)))

      // Anything directly right of
      exampleSquare1 should not(contain(Point(6, 3)))
      exampleSquare1 should not(contain(Point(6, 4)))
      exampleSquare1 should not(contain(Point(6, 5)))
      exampleSquare1 should not(contain(Point(6, 6)))
      exampleSquare1 should not(contain(Point(6, 7)))
      exampleSquare1 should not(contain(Point(6, 8)))
    }

    "intersect" in {
      exampleSquare1 should intersect(exampleSquare2)
      exampleSquare1 should intersect(exampleSquare4)
      exampleSquare1 should intersect(exampleSquare5)
    }
    "not intersect" in {
      exampleSquare1 should not(intersect(exampleSquare6))
      exampleSquare3 should not(intersect(exampleSquare1))
      exampleSquare3 should not(intersect(exampleSquare2))
    }
  }

  "Model" should {
    "parse examples" in {

      val square1 = "#1 @ 1,3: 4x4"
      val square2 = "#2 @ 3,1: 4x4"
      val square3 = "#3 @ 5,5: 2x2"

      Square.parse(square1) should contain (Square(1, Point(1, 3), Point(5, 7)))
      Square.parse(square2) should contain (Square(2, Point(3, 1), Point(7, 5)))
      Square.parse(square3) should contain (Square(3, Point(5, 5), Point(7, 7)))

    }
  }

  "Day 3" should {
    "solve example part 1" in {
      Day3.part1(Fabric(Set(exampleSquare1, exampleSquare2, exampleSquare3))) should be (4)
    }

    "solve example part 2" in {
      Day3.part2(Fabric(Set(exampleSquare1, exampleSquare2, exampleSquare3))) should be (Some(3))
    }
  }

}
